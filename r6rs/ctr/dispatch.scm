(library (ctr dispatch)
  (export dnf inject)
  (import (rnrs (6))

          (only (util) dovec vector-append inc)
          (only (util collections) mapv)
          
          (ctr ast))

  (define (dnf ast)
    (if (Primop? ast)
      (case (Primop-op ast)
        ((bior)
         ;; convert args and flatten the resulting `or` of `or`:s:
         (fold-dnfs (lambda (acc v)
                      (vector-append acc (Primop-args v)))
                    (wrap-connectives (make-Const #f))
                    (mapv dnf (Primop-args ast))))
        ((band)
         ;; convert args and distribute `and` over them:
         (fold-dnfs
          (lambda (acc v)
            (let* ((vargs (Primop-args v))
                   (res (make-vector (* (vector-length acc)
                                        (vector-length vargs))))
                   (i 0))
              (dovec (l acc)
                (dovec (r vargs)
                  (vector-set! res i
                               (make-Primop 'band
                                            (vector-append (Primop-args l)
                                                           (Primop-args r))
                                            #f))
                  (set! i (inc i))))
              res))
          (wrap-connectives (make-Const #t))
          (vector-map dnf (Primop-args ast))))
        ((bnot)
         ;; Use some Boolean algebra laws and reconvert:
         (let ((arg (vector-ref (Primop-args ast) 0)))
           (if (Primop? arg)
             (case (Primop-op arg)
               ((bior) ; De Morgan
                (dnf (make-Primop 'band
                                  (vector-map
                                   (lambda (v) (make-Primop 'bnot (vector v) #f))
                                   (Primop-args arg))
                                  #f)))
               ((band) ; De Morgan
                (dnf (make-Primop 'bior
                                  (vector-map
                                   (lambda (v) (make-Primop 'bnot (vector v) #f))
                                   (Primop-args arg))
                                  #f)))
               ((bnot) ; double negation
                (dnf (vector-ref (Primop-args arg) 0)))
               (else
                (wrap-connectives (make-Primop 'bnot (vector arg) #f))))
             (wrap-connectives (make-Primop 'bnot (vector arg) #f)))))
        (else
         (wrap-connectives ast)))
      (wrap-connectives ast)))

  (define (wrap-connectives node)
    (make-Primop 'bior (vector (make-Primop 'band (vector node) #f)) #f))

  (define (fold-dnfs f default dnfs)
    (case (vector-length dnfs)
      ((0)
       default)
      ((1)
       (vector-ref dnfs 0))
      (else
       (let ((len (vector-length dnfs)))
         (do ((acc (Primop-args (vector-ref dnfs 0)) (f acc (vector-ref dnfs i)))
              (i 1 (inc i)))
             ((>= i len)
              (make-Primop 'bior acc #f)))))))

  (define (inject ast)
    (if (Primop? ast)
      (case (Primop-op ast)
        ((bior band) (vector-map inject (Primop-args ast)))
        (else ast))
      ast)))

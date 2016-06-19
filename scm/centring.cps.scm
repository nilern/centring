(module centring.cps
  *

  (import scheme chicken)
  (use matchable
       persistent-hash-map
       sequences

       centring.ast
       (only centring.analyze dnf)
       (prefix centring.primops ops:))

  (define (cps-k ast c)
    (match ast
      (($ Primop op args _ ann)
       (cps-primop op args ann c))
      (($ Do stmts _)
       (cps-stmts stmts c))
       
      ((? Const?) (c ast))
      ((? Global?) (c ast))
      ((? Local?) (c ast))

      (_ (error "unable to CPS convert" ast))))

  (define (cps-primop op args ann c)
    (let ((purpose (ops:op-purpose op)))
      (case purpose
        ((expr)
         (cps-args
          args
          (lambda (as)
            (let ((res (gensym 'v)))
              (Primop op as
                      (vector
                       (Fn (vector res)
                           (vector
                            (vector
                             (dnf (Const #t (persistent-map)))
                             (c (Local res (persistent-map)))))
                           (persistent-map)))
                      ann)))))
        (else (error "unable to convert primop with purpose" purpose)))))

  (define (cps-stmts stmts c)
    (match stmts
      (#()
       (cps-k
        (Primop 'rec
                (vector (Global 'centring.lang #f 'Tuple (persistent-map)))
                #f (persistent-map))
        c))
      (#(stmt)
       (cps-k stmt c))
      (_
       (cps-k
        (foldl
         (lambda (acc stmt)
           (Primop 'apply
                   (vector (Fn (gensym '_)
                               (vector (dnf (Const #t (persistent-map)))
                                       stmt)
                               (persistent-map)))
                   #f
                   acc))
         (peek stmts)
         (pop stmts)) c))))

  (define (cps-args vec k)
    (let* ((len (vector-length vec))
           (res (make-vector len)))
      (define (cpsv i)
        (if (= i len)
          (k res)
          (cps-k (vector-ref vec i)
                 (lambda (v) (vector-set! res i v) (cpsv (add1 i))))))
      (cpsv 0))))

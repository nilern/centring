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
      (($ Primop op args _)
       (cps-primop op args c))
      (($ Do stmts)
       (cps-stmts stmts c))
       
      ((? Const?) (c ast))
      ((? Global?) (c ast))
      ((? Local?) (c ast))

      (_ (error "unable to CPS convert" ast))))

  (define (cps-primop op args c)
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
                             (dnf (Const #t))
                             (c (Local res))))
                           #f)))))))
        ((stmt)
         (cps-args
          args
          (lambda (as)
            (Primop op as
                    (vector
                     (Fn #()
                         (vector
                          (vector
                           (dnf (Const #t))
                           (cps-k
                            (Primop 'rec
                                    (vector (Global 'centring.lang
                                                    'centring.lang
                                                    'Tuple))
                                    #f)
                            c)))
                         #f))))))
        (else (error "unable to convert primop with purpose" purpose)))))

  (define (cps-stmts stmts c)
    (match stmts
      (#()
       (cps-k
        (Primop 'rec
                (vector (Global 'centring.lang #f 'Tuple))
                #f)
        c))
      (#(stmt)
       (cps-k stmt c))
      (_
       (cps-k
        (foldl
         (lambda (acc stmt)
           (Primop 'apply
                   (vector (Fn (gensym '_)
                               (vector (dnf (Const #t))
                                       stmt)
                               #f)
                           acc)
                   #f))
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

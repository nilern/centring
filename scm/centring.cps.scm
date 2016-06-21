(module centring.cps
  *

  (import scheme chicken)
  (use matchable
       persistent-hash-map
       sequences

       centring.ast
       centring.util
       (only centring.analyze dnf)
       (prefix centring.primops ops:))

  (define (cps-k ast c)
    (match ast
      ((? Fn?)
       (let ((f (gensym 'f)))
         (cps-k (Fix (vector (cons f ast)) (Local f)) c)))
      (($ Fix bindings body)
       (Fix (mapv (match-lambda ((var . f) (cons var (cps-fn f))))
                  bindings)
            (cps-k body c)))
      (($ Primop op args _)
       (cps-primop op args c))
      (($ Do stmts)
       (cps-stmts stmts c))
       
      ((? Const?) (c ast))
      ((? Global?) (c ast))
      ((? Local?) (c ast))

      (_ (error "unable to CPS convert" ast))))

  (define (cps-fn f)
    (match-let* ((($ Fn arg cases _) f)
                 (ret (gensym 'r)))
      (Fn (vector arg ret)
          (mapv (match-lambda
                 (#(cond body)
                  (vector (cps-condition cond)
                          (cps-k body
                                 (lambda (v)
                                   (Primop 'continue (vector (Local ret) v) #()))))))
                cases)
          #f)))

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
        ((ctrl)
         (case op
           ((apply)
            (match-let ((#(callee arg) args))
              (cps-k
               callee
               (lambda (f)
                 (let* ((ret (gensym 'r))
                        (res (gensym 'v)))
                   (Fix (vector
                         (cons ret (Fn (vector res)
                                       (vector (vector (dnf (Const #t))
                                                       (c (Local res))))
                                       #f)))
                        (cps-k
                         arg
                         (lambda (a)
                           (Primop 'apply (vector f a (Local ret)) #())))))))))
           ((halt)
            (match-let ((#(v) args))
              (Primop 'halt (vector v) #())))
           (else
            (error "unable to convert control operation" op))))
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
                               (vector (vector (dnf (Const #t)) stmt))
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
      (cpsv 0)))

  (define (cps-condition cond)
    (define cps-or
      (match-lambda
       (($ Primop 'bior ands _)
        (Primop 'bior (mapv cps-and ands) #f))))
    (define cps-and
      (match-lambda
       (($ Primop 'band els _)
        (Primop 'band (mapv cps-el els) #f))))
    (define cps-el
      (match-lambda
       (($ Primop 'bnot #(at) _)
        (Primop 'bnot (vector (cps-el at)) #f))
       (at
        (cps-k at (lambda (v) (Primop 'yield (vector v) #()))))))
    (cps-or cond)))

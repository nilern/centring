(module centring.cps
  *

  (import scheme chicken)
  (use matchable

       centring.schring
       (prefix centring.analyze ana:))

  (define-enum CPS
    (Fn formals types body)
    (Cont formals types body)
    (Fix bindings body)
    (Primop op args conts)

    (Splat val)
    (Global ns name)
    (Clover index)
    (Local name)
    (Const val))

  ;;;; CPS transform

  (define (ast->cps ast)
    (define (cps-k ast k)
      (match ast
        (($ ana:AConst val) (k (Const val)))))
    (cps-k ast (lambda (v) (Primop 'halt v #())))))

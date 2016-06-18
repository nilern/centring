(module centring.cps
  *

  (import scheme chicken)
  (use matchable

       centring.ast)

  (define (cps-k ast c)
    (match ast
      ((? Const?) (c ast))
      ((? Global?) (c ast))
      ((? Local?) (c ast)))))

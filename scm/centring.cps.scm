(module centring.cps
  *

  (import scheme chicken)
  (use matchable
       persistent-hash-map

       centring.ast)

  (define (cps-k ast c)
    (match ast
      (($ Do #(stmt) _)
       (cps-k stmt c))
      (($ Do #() ann)
       (cps-k
        (Primop 'rec
                (vector (Global 'centring.lang #f 'Tuple (persistent-map))) ann)
        c))
       
      ((? Const?) (c ast))
      ((? Global?) (c ast))
      ((? Local?) (c ast)))))

(module centring.cps
  *

  (import scheme chicken)

  (define-record Block label formals types body)

  (define-record Local name)
  (define-record Clover index)
  (define-record Constant val)
  (define-record Global name))

(module centring.util
  *

  (import scheme chicken)
  (use (only clojurian-syntax ->))

  (define (keyword->symbol kw)
    (-> kw keyword->string string->symbol)))

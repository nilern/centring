(library (util collections)
  (export first rest empty?)
  (import (rnrs (6)))

  (define (first coll)
    (car coll))

  (define (rest coll)
    (cdr coll))

  (define (empty? coll)
    (null? coll)))

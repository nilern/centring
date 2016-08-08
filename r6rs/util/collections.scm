(library (util collections)
  (export first rest empty?
          conj
          reduce transduce
          into)
  (import (rnrs (6)))

  ;;;;

  (define (first coll)
    (car coll))

  (define (rest coll)
    (cdr coll))

  (define (empty? coll)
    (null? coll))

  ;;;;

  (define (conj coll v)
    (cons v coll))

  ;;;;

  (define (reduce f v coll)
    (fold-left f v coll))

  (define (transduce xform f v coll)
    (reduce (xform f) v coll))

  ;;;;

  (define (into coll other)
    (reduce conj coll other)))

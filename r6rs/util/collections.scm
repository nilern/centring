(library (util collections)
  (export first rest empty?
          conj
          reduce transduce
          into
          drop last drop-last)
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
    (reduce conj coll other))

  ;;;;

  (define (drop n coll)
    (cond
     ((null? coll) coll)
     ((zero? n) coll)
     (else (drop (- n 1) (rest coll)))))

  (define (last coll)
    (if (empty? (rest coll))
      (first coll)
      (last (rest coll))))

  (define (drop-last n coll)
    ;; TODO: optimize
    (reverse (drop n (reverse coll)))))

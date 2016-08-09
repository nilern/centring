(library (util collections)
  (export first rest empty?
          conj
          reduce transduce
          into
          mapv mapl)
  (import (rnrs (6))

          (only (util) doto))

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

  (define (mapv f coll)
    (define (do-map coll i)
      (if (empty? coll)
        (make-vector i)
        (doto (do-map (rest coll) (+ i 1))
          (vector-set! i (f (first coll))))))
    (do-map coll 0))

  (define (mapl f coll)
    (let recur ((i (- (vector-length coll) 1)) (res '()))
      (if (< i 0)
        res
        (recur (- i 1) (cons (f (vector-ref coll i)) res))))))
        

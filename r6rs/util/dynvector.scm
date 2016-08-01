(library (util dynvector)
  (export make-dynvector
          dynvector-ref dynvector-length
          dynvector-set! dynvector-push!)
  (import (rnrs (6))

          (only (util) inc))

  (define-record-type DynVector
    (fields
     (mutable data dynvector-data dynvector-data-set!)
     (mutable length dynvector-length dynvector-length-set!)))

  (define (make-dynvector)
    (make-DynVector '#() 0))

  (define (dynvector-reserve! dvec n)
    (let* ((data (dynvector-data dvec))
           (len (dynvector-length dvec))
           (cap (vector-length data))
           (cap* (max (* 2 len) n 16))
           (data* (make-vector cap*)))
      ;; TODO: use `vector-copy!`:
      (let recur ((i 0))
        (when (< i len)
          (vector-set! data* i (vector-ref data i))
          (recur (inc i))))
      (dynvector-data-set! dvec data*)))

  (define (dynvector-ref dvec i)
    (assert (< i (dynvector-length dvec)))
    (vector-ref (dynvector-data dvec) i))

  (define (dynvector-set! dvec i v)
    (assert (< i (dynvector-length dvec)))
    (vector-set! (dynvector-data dvec) i v))

  (define (dynvector-push! dvec v)
    (let* ((len (dynvector-length dvec))
           (len* (inc len)))
      (dynvector-reserve! dvec len*)
      (dynvector-length-set! dvec len*)
      (dynvector-set! dvec len v))))


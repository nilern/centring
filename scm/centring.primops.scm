(module centring.primops
  *
        
  (import scheme chicken)

  (use (srfi 69)
       matchable
       (only miscmacros define-syntax-rule)
       (only clojurian-syntax ->))

  ;;;;

  (define primops (make-hash-table))

  (define-syntax-rule (define-primop (name itp argpat) body ...)
    (begin
      (define (name itp argv)
        (match-let ((argpat argv))
          body ...))
      (hash-table-set! primops (quote name) name)))

  ;;;;

  (define-primop (rec _ argv)
    argv)

  (define-primop (rref _ #(rec i))
    (vector-ref rec (add1 i)))

  ;; TODO: return empty tuple
  (define-primop (rset! _ #(rec i v))
    (vector-set! rec (add1 i) v))

  (define-primop (rlen _ #(rec))
    (sub1 (vector-length rec)))

  ;; TODO: types of primitives (fixnums, booleans etc.)
  (define-primop (type _ #(#(t _ ...)))
    t)

  (define-primop (bit-eq? _ #(a b))
    (eq? a b)))

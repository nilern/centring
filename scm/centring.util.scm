(module centring.util
  *

  (import scheme chicken)
  (use (only (srfi 13) string-index)
       (srfi 69)
       vector-lib
       sequences
       (only miscmacros define-syntax-rule))

  (define (ns-name sym)
    (let* ((symstr (symbol->string sym))
           (i (string-index symstr #\/)))
      (if (or (not i) (= i 0) (= i (sub1 (string-length symstr))))
        (values #f sym)
        (values (string->symbol (substring symstr 0 i))
                (string->symbol (substring symstr (add1 i)))))))

  (define (name sym)
    (receive (_ name) (ns-name sym) name))

  (define (ns sym)
    (receive (ns _) (ns-name sym) ns))

  (define ns-sep '/)

  (define (literal? v)
    (or (fixnum? v) (boolean? v)))

  (define mapv (cute smap #() <> <>))

  (define (map-pair f p)
    (cons (f (car p)) (f (cdr p))))

  (define (hash-table-zip ks vs)
    (let ((res (make-hash-table)))
      (vector-for-each
       (lambda (_ k v)
         (hash-table-set! res k v))
       ks vs)
      res))

  (define-syntax-rule (doseq (v coll) body ...)
    (for (lambda (v) body ...) coll))

  (define-syntax doseq
    (syntax-rules ()
      ((doseq ((v i) coll) body ...)
       (for* (lambda (coll it) (let ((i (index it)) (v (elt coll it))) body ...)) coll))
      ((doseq (v coll) body ...)
       (for (lambda (v) body ...) coll)))))

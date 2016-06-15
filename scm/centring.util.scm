(module centring.util
  *

  (import scheme chicken)
  (use (only (srfi 13) string-index)
       sequences
       coops coops-primitive-objects
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

  (define mapv (cute smap #() <> <>))

  (define-syntax-rule (doseq (v coll) body ...)
    (for (lambda (v) body ...) coll)))

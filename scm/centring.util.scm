(module centring.util
  (ns name ns-name doseq fmap-st)

  (import scheme chicken)
  (use (only (srfi 13) string-index)
       (only sequences for)
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

  (define-syntax-rule (doseq (v coll) body ...)
    (for (lambda (v) body ...) coll))

  (define-generic (fmap-st f coll ist))

  (define-method (fmap-st (f #t) (coll <list>) ist)
    (let recur ((coll coll) (st ist))
      (if (null? coll)
        (values coll st)
        (receive (v st*) (f (car coll) st)
          (receive (vs st**) (recur (cdr coll) st*)
            (values (cons v vs) st**))))))

  (define-method (fmap-st (f #t) (coll <vector>) ist)
    (let* ((len (vector-length coll))
           (res (make-vector len))
           (st ist))
      (do ((i 0 (add1 i))) ((= i len))
        (receive (v st*) (f (vector-ref coll i) st)
          (vector-set! res i v)
          (set! st st*)))
      (values res st))))

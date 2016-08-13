(library (ctr util)
  (export ctr-path ns-sep file-ending
          literal?
          ns-name ns name
          ctr-error ctr-error?)
  (import (rnrs (6))
          (only (chezscheme) make-parameter)

          (only (util) string-index inc dec))

  (define ctr-path (make-parameter '()))

  (define ns-sep '/)

  (define file-ending ".ctr")

  (define (literal? v)
    (or (symbol? v) (fixnum? v) (flonum? v) (boolean? v) (char? v)))

  (define (ns-name sym)
    (let* ((symstr (symbol->string sym))
           (i (string-index #\/ symstr))
           (len (string-length symstr)))
      (if (or (not i) (= i 0) (= i (dec len)))
        (values #f sym)
        (values (string->symbol (substring symstr 0 i))
                (string->symbol (substring symstr (inc i) len))))))

  (define (ns sym)
    (let-values (((ns _) (ns-name sym)))
      ns))

  (define (name sym)
    (let-values (((_ name) (ns-name sym)))
      name))

  (define-condition-type &ctr &condition make-ctr-error ctr-error?)

  (define (ctr-error msg . irritants)
    (raise
     (condition
      (make-ctr-error)
      (make-message-condition msg)
      (make-irritants-condition irritants)))))

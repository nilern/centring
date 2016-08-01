(library (util)
  (export let-cc if-let when-let defrecord
          symbol-append
          identity
          inc dec)
  (import (rnrs (6)))

  ;;;;

  (define-syntax let-cc
    (syntax-rules ()
      ((let-cc k body ...)
       (call-with-current-continuation
        (lambda (k)
          body ...)))))

  (define-syntax if-let
    (syntax-rules ()
      ((if-let (v expr) then else)
       (let ((v* expr))
         (if v*
           (let ((v v*))
             then)
           else)))))

  (define-syntax when-let
    (syntax-rules ()
      ((when-let (v expr) then ...)
       (let ((v expr))
         (when v
           then ...)))))

  ;;;;

  (define-syntax defrecord
    (syntax-rules ()
      ((defrecord (T field ...))
       (define-record-type T
         (fields (immutable field) ...)))))

  ;;;;

  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))

  ;;;;

  (define (identity x) x)

  ;;;;

  (define (inc n) (+ n 1))
  (define (dec n) (- n 1)))

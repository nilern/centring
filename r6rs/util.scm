(library (util)
  (export let-cc if-let when-let doto dolist defrecord
          string-index symbol-append
          identity every-pred some-fn complement partial comp
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

  (define-syntax doto
    (syntax-rules ()
      ((doto expr (op arg ...) ...)
       (let ((v expr))
         (begin (op v arg ...) ...)
         v))))

  (define-syntax dolist
    (syntax-rules ()
      ((dolist ((v i) coll) body ...)
       (let recur ((i 0) (ls coll))
         (unless (null? ls)
           (let ((v (car ls)))
             body ...)
           (recur (+ i 1) (cdr ls)))))
      ((dolist (v coll) body ...)
       (for-each (lambda (v) body ...) coll))))

  ;;;;

  (define-syntax defrecord
    (syntax-rules ()
      ((defrecord (T field ...))
       (define-record-type T
         (fields (immutable field) ...)))))

  ;;;;

  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))

  (define (string-index c s)
    (let ((len (string-length s)))
      (let recur ((i 0))
        (cond
         ((>= i len) #f)
         ((char=? (string-ref s i) c) i)
         (else (recur (inc i)))))))

  ;;;;

  (define (identity x) x)

  (define (complement pred?)
    (lambda xs
      (not (apply pred? xs))))

  (define (every-pred . preds)
    (lambda xs
      (let recur ((preds preds))
        (cond
         ((null? preds) #t)
         ((apply (car preds) xs) (recur (cdr preds)))
         (else #f)))))

  (define (some-fn . preds)
    (lambda xs
      (let recur ((preds preds))
        (cond
         ((null? preds) #f)
         ((apply (car preds) xs) #t)
         (else (recur (cdr preds)))))))

  (define (partial f . xs)
    (lambda ys
      (apply f (append xs ys))))

  (define (comp . fs)
    (lambda (arg)
      (fold-right (lambda (f acc) (f acc)) arg fs)))

  ;;;;

  (define (inc n) (+ n 1))
  (define (dec n) (- n 1)))

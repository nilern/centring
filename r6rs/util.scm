(library (util)
  (export let-cc if-let when-let doto -> ->> dolist dovec defrecord
          string-index string-split vector-append symbol-append
          identity every-pred some-fn complement partial comp
          inc dec
          write-line)
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

  (define-syntax ->
    (lambda (stx)
      (syntax-case stx ()
        ((-> expr f wraps ...) (identifier? #'f)
         #'(-> (f expr) wraps ...))
        ((-> expr (f args ...) wraps ...)
         #'(-> (f expr args ...) wraps ...))
        ((-> expr)
         #'expr)
        ((->)
         #'(begin)))))

  (define-syntax ->>
    (lambda (stx)
      (syntax-case stx ()
        ((->> expr f wraps ...) (identifier? #'f)
         #'(->> (f expr) wraps ...))
        ((->> expr (f args ...) wraps ...)
         #'(->> (f args ... expr) wraps ...))
        ((->> expr)
         #'expr)
        ((->>)
         #'(begin)))))

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

  (define-syntax dovec
    (syntax-rules ()
      ((_ (v coll) body ...)
       (let* ((vec coll)
              (len (vector-length vec)))
         (do ((i 0 (inc i)))
             ((>= i len))
           (let ((v (vector-ref vec i)))
             body ...))))))

  ;;;;

  (define-syntax defrecord
    (syntax-rules ()
      ((defrecord (T field ...))
       (define-record-type T
         (fields (immutable field) ...)))))

  ;;;;

  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))

  (define (vector-append . vecs)
    (let ((res (make-vector (fold-left (lambda (n vec) (+ n (vector-length vec)))
                                       0 vecs)))
          (i 0))
      (dolist (vec vecs)
        (dovec (v vec)
          (vector-set! res i v)
          (set! i (inc i))))
      res))

  (define string-index
    (case-lambda
     ((c s)
      (string-index c 0 s))
     ((c o s)
      (let ((len (string-length s)))
        (let recur ((i o))
          (cond
           ((>= i len) #f)
           ((char=? (string-ref s i) c) i)
           (else (recur (inc i)))))))))

  (define (string-split c str)
    (let ((len (string-length str)))
      (let recur ((i -1) (res '()))
        (if (>= i len)
          (reverse res)
          (let* ((min-i (inc i))
                 (max-i (or (string-index c min-i str) len)))
            (if (> max-i min-i)
              (recur max-i (cons (substring str min-i max-i) res))
              (recur max-i res)))))))

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
  (define (dec n) (- n 1))

  ;;;;

  (define write-line
    (case-lambda
     ((obj)
      (write-line obj (current-output-port)))
     ((obj port)
      (write obj port)
      (write-char #\newline port)))))

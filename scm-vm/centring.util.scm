(module centring.util
  *

  (import scheme chicken)
  (use (only clojurian-syntax ->))

  (define (keyword->symbol kw)
    (-> kw keyword->string string->symbol))

  (define-record-type stack
    (raw-stack stack start end buffer)
    stack?
    
    (start stack-start stack-start-set!)
    (end stack-end stack-end-set!)
    (buffer stack-buffer stack-buffer-set!))

  (define (stack-has-space? st)
    (< (stack-end st) (vector-length (stack-buffer st))))

  (define (stack-push! st v)
    (if (stack-has-space? st)
      (begin
        (vector-set! st (stack-end st) v)
        (stack-end-set! st (add1 (stack-start st))))
      (error "stack not finished!"))))

(define-library (centring util)
  (export gensym keyword? keyword->symbol comp)
  
  (import (scheme base)
          (srfi 1))

  (begin
    (define gensym
      (let ((counter 0))
        (lambda (sym)
          (let ((c counter))
            (set! counter (+ counter 1))
            (string->symbol
             (string-append (symbol->string sym) (number->string c)))))))
    
    (define (keyword? sym)
      (and (symbol? sym) (eq? (string-ref (symbol->string sym) 0) #\:)))
    
    (define (keyword->symbol kw)
      (string->symbol (substring (symbol->string kw) 1)))

    (define (comp . fs)
      (lambda (v)
        (fold (lambda (acc f) (f acc)) v fs)))))

(use (srfi 69)
     (only data-structures identity)
     matchable)

(define-record environment
  table
  parent)

(define (env-def env name value)
  (hash-table-set! (environment-table env) name value))

(define (env-ref env name)
  (cond 
    ((hash-table-ref/default (environment-table env) name #f) => identity)
    ((environment-parent env) (env-ref (environment-parent env) name))
    (else #f)))

(define (evaluate expr env)
  (match expr
    (('quote e) e)
    (('car l) (car (evaluate l env)))))

(define (main)
  (write (evaluate '(car (quote (2 3)))
                   (make-environment (make-hash-table) #f))))

(main)

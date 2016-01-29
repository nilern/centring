(use (srfi 1 69)
     (only extras sprintf read-file write-line)
     (only data-structures identity)
     (only matchable match)
     (only anaphora acond))

;;;; Environments

(define-record environment
  table
  parent)

(define (env-ref env name)
  (acond
    ((hash-table-ref/default (environment-table env) name #f) it)
    ((environment-parent env) (env-ref it name))
    (else (error "can't reference unbound variable" name))))

(define (env-def! env name value)
  (hash-table-set! (environment-table env) name value))

(define (env-set! env name value)
  (acond 
    ((hash-table-exists? (environment-table env) name)
     (env-def! env name value))
    ((environment-parent env)
     (env-set! it name value))
    (else
     (error (sprintf "can't `set!` an undefined identifier `~S` to `~S`"
                     name value)))))

;;;; Eval

(define (truthy? v)
  (not (or (eq? v #f) (null? v))))

(define (expand expr)
  (match expr
    (('def (name . formals) . body) `(def ,name (fn ,formals ,@body)))
    (_ expr)))

(define (evaluate expr env)
  (match (expand expr)
    ((or (? number? e) (? string? e)) ; literals
     e)
    (('quote e) ; quoting
     e)

    ((? symbol? identifier) ; loading
     (env-ref env identifier))
    (('def (? symbol? name) expr) ; definition
     (env-def! env name (evaluate expr env)))
    (('set! name expr) ; assignment
     (env-set! env name (evaluate expr env)))

    (('if cond then else) ; conditional evaluation
     (if (truthy? (evaluate cond env))
       (evaluate then env)
       (evaluate else env)))
    (('do . statements) ; imperative block
     (fold (lambda (stmt _) (evaluate stmt env)) '() statements))

    (('fn formals . body) ; function creation
     (lambda args (evaluate `(do ,@body)
                            (make-environment
                              (alist->hash-table (map cons formals args))
                              env))))
    ((op . args) ; call
     (apply (evaluate op env) (map (cute evaluate <> env) args)))))

;;;; Main

(define (main)
  (write (evaluate `(do ,@(read-file (second (argv))))
                   (make-environment (alist->hash-table
                                       `((car . ,car)
                                         (+ . ,+)
                                         (- . ,-)
                                         (* . ,*)
                                         (< . ,<)
                                         (writeLn . ,write-line)
                                         (number->String . ,number->string)))
                                     #f))))

(main)

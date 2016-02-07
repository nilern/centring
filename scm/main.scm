;; (use #|(srfi 1 69)
;;      (only extras sprintf read-file write-line pretty-print)
;;      (only data-structures identity)
;;      (only matchable match)
;;      (only anaphora acond)|#
;;      coops)

(use coops
     matchable
     (only anaphora acond)
     (srfi 69))

;;;;

(define-generic (first coll))
(define-generic (rest coll))

(define (second coll) (first (rest coll)))

;;;;

(define-class Value)

(define-class SelfEval (Value))

(define-class Int (SelfEval)
  (val))

(define-class Symbol (Value)
  ((ns :reader ns)
   (name :reader name)))

(define-class Pair (Value)
  ((car :reader first)
   (cdr :reader rest)))

(define-class EmptyList (Value))

(define-class NilClass (Value))

(define-class Env ()
  (table
   parent))

;;;;

(define empty-list (make EmptyList))

(define Nil (make NilClass))

;;;;

(define (analyze sexpr ns-name)
  (match sexpr
    ((? integer?) (make Int 'val sexpr))
    ((? symbol?)  (make Symbol 'ns ns-name 'name sexpr))
    ((v . vs)     (make Pair 'car (analyze v ns-name)
                             'cdr (analyze vs ns-name)))
    ('()          empty-list)))

;;;;

(define-method (lookup (env Env) sym)
  (let ((name (name sym)))
    (acond
      ((hash-table-ref/default (slot-value env 'table) name #f) it)
      ((slot-value env 'parent) (lookup it name))
      (else (error "can't reference unbound variable" name)))))

(define (self-eval? ast)
  (subclass? (class-of ast) SelfEval))

(define (varname? ast)
  (eq? (class-of ast) Symbol))

(define (quote? ast)
  (eq? (name (first ast)) 'quote))

(define-method (empty? (ast EmptyList)) #t)
(define-method (empty? (ast #t)) #f)

(define (do? ast)
  (eq? (name (first ast)) 'do))

;;;;

(define (evaluate-block ast env k)
  (cond
    ((empty? ast)
     (k Nil))
    ((empty? (rest ast))
     (evaluate (first ast) env k))
    (else
     (evaluate (first ast) env
               (lambda (_) (evaluate-block (rest ast) env k))))))

(define (evaluate ast env k)
  (match ast
    ((? self-eval?) (k ast))
    ((? quote?)     (k (second ast)))

    ((? varname?)   (k (lookup env ast)))

    ((? do?)        (evaluate-block (rest ast) env k))))

;;;; Environments

;; (define-record environment
;;   table
;;   parent)
;; 
;; (define (env-ref env name)
;;   (acond
;;     ((hash-table-ref/default (environment-table env) name #f) it)
;;     ((environment-parent env) (env-ref it name))
;;     (else (error "can't reference unbound variable" name))))
;; 
;; (define (env-def! env name value)
;;   (hash-table-set! (environment-table env) name value))
;; 
;; (define (env-set! env name value)
;;   (acond 
;;     ((hash-table-exists? (environment-table env) name)
;;      (env-def! env name value))
;;     ((environment-parent env)
;;      (env-set! it name value))
;;     (else
;;      (error (sprintf "can't `set!` an undefined identifier `~S` to `~S`"
;;                      name value)))))

;;;; Eval

;; (define (truthy? v)
;;   (not (or (eq? v #f) (null? v))))
;; 
;; (define (expand expr)
;;   (match expr
;;     (('def (and (? proper-list?) (name . formals)) . body)
;;      `(def ,name (fn ,formals ,@body)))
;;     (('def (name . formal) . body)
;;      `(def ,name (fn ,formal ,@body)))
;;     (_ expr)))

;; (define (evaluate expr env)
;;   (match (expand expr)
;;     ((or (? number? e) (? string? e)) ; literals
;;      e)
;;     (('quote e) ; quoting
;;      e)
;; 
;;     ((? symbol? identifier) ; loading
;;      (env-ref env identifier))
;;     (('def (? symbol? name) expr) ; definition
;;      (env-def! env name (evaluate expr env)))
;;     (('set! name expr) ; assignment
;;      (env-set! env name (evaluate expr env)))
;; 
;;     (('if cond then else) ; conditional evaluation
;;      (if (truthy? (evaluate cond env))
;;        (evaluate then env)
;;        (evaluate else env)))
;;     (('do . statements) ; imperative block
;;      (fold (lambda (stmt _) (evaluate stmt env)) '() statements))
;; 
;;     (('fn (and (? proper-list?) formals) . body) ; function creation
;;      (lambda args (evaluate `(do ,@body)
;;                             (make-environment
;;                               (alist->hash-table (map cons formals args))
;;                               env))))
;;     (('fn formal . body)
;;      (lambda args (evaluate `(do ,@body)
;;                             (make-environment
;;                               (alist->hash-table `((,formal . ,args)))
;;                               env))))
;;     ((op . args) ; call
;;      (apply (evaluate op env) (map (cute evaluate <> env) args)))))
;; 
;; ;;;; Main
;; 
;; (define (main)
;;   (write (evaluate `(do ,@ (read-file (second (argv))))
;;                    (make-environment (alist->hash-table
;;                                        `((car . ,car)
;;                                          (cdr . ,cdr)
;;                                          (cons . , cons)
;;                                          (null? . ,null?)
;;                                          (+ . ,+)
;;                                          (- . ,-)
;;                                          (* . ,*)
;;                                          (< . ,<)
;;                                          (writeln . ,write-line)
;;                                          (symbol? . ,symbol?)
;;                                          (number->string . ,number->string)
;;                                          (pretty-print . ,pretty-print)))
;;                                      #f))))
;; 
;; (main)

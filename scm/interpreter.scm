(include "bootstrap.scm")
(import centring.bootstrap)
(use coops coops-primitive-objects
     (only miscmacros let/cc)
     (only anaphora acond)
     (srfi 69))

;;;;

(define-generic (first coll))
(define-generic (rest coll))

(def (second coll) (first (rest coll)))
(def (third coll) (first (rest (rest coll))))
(def (fourth coll) (first (rest (rest (rest coll)))))

;;;;

(defenum Value
  (Int val)
  (Bool val)
  (EmptyList)
  (Nil)
  (Symbol ns name)
  (Pair car cdr)
  (Closure formals code env)
  (NativeFn fn))

(define-class Env ()
  (table
   parent))

;;;;

(def empty-list (Value/EmptyList))
(def nil (Value/Nil))

;;;;

(defmethod (first ls :Value/Pair) (slot-value ls 'car))
(defmethod (rest ls :Value/Pair) (slot-value ls 'cdr))

(defmethod (name sym :Value/Symbol) (slot-value sym 'name))

(def (Pair->list p)
  (if (empty? p)
    '()
    (cons (first p) (Pair->list (rest p)))))

(def (numeric-binop res-type f)
  (Value/NativeFn
    (lambda (a b)
      (res-type
        (f (slot-value a 'val)
           (slot-value b 'val))))))

;;;;

(def (analyze sexpr)
  (match sexpr
    (? fixnum?) (Value/Int sexpr)
    (? boolean?) (Value/Bool sexpr)
    (? symbol?)  (Value/Symbol #f sexpr)
    (v . vs)     (Value/Pair (analyze v) (analyze vs))
    '()          empty-list))

;;;;

(define-method (lookup (env Env) sym)
  (let ((name (name sym)))
    (acond
      ((hash-table-ref/default (slot-value env 'table) name #f) it)
      ((slot-value env 'parent) (lookup it sym))
      (else (error "can't reference unbound variable" name)))))

(define-method (extend (env Env) sym value)
  (let ((name (name sym)))
    (hash-table-set! (slot-value env 'table) name value)))

(def (empty? ast)
  (eq? ast empty-list))

(def (truthy? ast)
  (not
    (or (eq? ast nil)
        (and (Value/Bool? ast)
             (not (slot-value ast 'val))))))

;;;;

(def (evaluate-block ast env k)
  (cond
    ((empty? ast)
     (k nil))
    ((empty? (rest ast))
     (evaluate (first ast) env k))
    (else
     (evaluate (first ast) env
               (lambda (_) (evaluate-block (rest ast) env k))))))

(def (evaluate-if ast env k)
  (evaluate (second ast) env
            (lambda (cv)
              (if (truthy? cv)
                (evaluate (third ast) env k)
                (evaluate (fourth ast) env k)))))

(def (evaluate-args args evargs env k)
  (evaluate (first args) env
            (lambda (v)
              (let ((evargs (cons v evargs)))
                (if (empty? (rest args))
                  (k (reverse evargs))
                  (evaluate-args (rest args) evargs env k))))))

(def (evaluate-call ast env k)
  (evaluate (first ast) env
            (lambda (f)
              (evaluate-args
                (rest ast) '() env
                (lambda (evargs)
                  (if (Value/NativeFn? f)
                    (k (apply (slot-value f 'fn) evargs))
                    (evaluate (slot-value f 'code)
                              (make Env
                                'table (alist->hash-table
                                        (map (lambda (k v) `(,(name k) . ,v))
                                              (Pair->list (slot-value f 'formals))
                                              evargs))
                                'parent (slot-value f 'env))
                              k)))))))

(def (evaluate ast env k)
  (match ast
    (or (? Value/Int?) (? Value/Bool?) (? Value/EmptyList?) (? Value/Nil?))
    (k ast)

    (? Value/Symbol?)
    (k (lookup env ast))

    (? Value/Pair?)
    (match (name (first ast))
      'quote (k (second ast))
      'def   (evaluate (third ast) env
                       (lambda (v) (extend env (second ast) v) (k nil)))
      'do    (evaluate-block (rest ast) env k)
      'if    (evaluate-if ast env k)
      'fn    (k (Value/Closure (second ast) (third ast) env))
      _      (evaluate-call ast env k))))

;;;;

(def (main)
  (writeln (slot-value
    (let/cc k
      (evaluate (analyze '(do
                            (def fact
                              (fn (n)
                                (if (< n 1)
                                  1
                                  (* n (fact (- n 1))))))
                            (fact 5)))
                (make Env
                  'table (alist->hash-table
                           `((< . ,(numeric-binop Value/Bool <))
                             (* . ,(numeric-binop Value/Int *))
                             (- . ,(numeric-binop Value/Int -))))
                  'parent #f)
                k)) 'val)))

(main)

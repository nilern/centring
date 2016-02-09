(include "bootstrap.scm")

(module centring.interpreter
  (Value/Int
   Value/Bool
   Value/NativeFn
   Env
   nil

   analyze
   denotate
   evaluate

   native-fn
   numeric-binop

   centring.core)

  (import scheme chicken
          centring.bootstrap)
  (use coops coops-primitive-objects
       (only miscmacros let/cc define-syntax-rule)
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
    (NativeFn fn)
    (Ns name bindings refers aliases)
    (Box ref))

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

  (define-syntax-rule (native-fn formals body ...)
    (Value/NativeFn
      (lambda formals
        body ...)))

  (def (numeric-binop res-type f)
    (native-fn (a b)
      (res-type
        (f (slot-value a 'val)
           (slot-value b 'val)))))

  ;;;;

  (def (analyze sexpr)
    (match sexpr
      (? fixnum?) (Value/Int sexpr)
      (? boolean?) (Value/Bool sexpr)
      (? symbol?)  (Value/Symbol nil sexpr)
      (v . vs)     (Value/Pair (analyze v) (analyze vs))
      '()          empty-list))

  (def (denotate ast)
    (match ast
      (? Value/Int?)       (slot-value ast 'val)
      (? Value/Bool?)      (slot-value ast 'val)
      (? Value/EmptyList?) '()
      (? Value/Nil?)       ast
      (? Value/Symbol?)    (if (slot-value ast 'ns)
                             (symbol (name (slot-value ast 'ns))
                                     (name (slot-value ast 'name)))
                             (slot-value ast 'name))
      (? Value/Pair?)      (cons (denotate (first ast)) (denotate (rest ast)))
      (? Value/Closure?)   ast
      (? Value/NativeFn?)  (slot-value ast 'fn)
      (? Value/Ns?)        ast
      (? Value/Box?)       ast))

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

  (defmethod (lookup ns :Value/Ns sym)
    (let ((sym-ns (slot-value sym 'ns))
          (name (name sym)))
      (cond
        ((eq? sym-ns nil)
         (or
           (hash-table-ref/default (slot-value ns 'bindings) name #f)
           (hash-table-ref/default (slot-value ns 'refers) name #f)
           (error "can't reference unbound variable" name)))
        ((eq? sym-ns (slot-value ns 'name))
         (or
           (hash-table-ref/default (slot-value ns 'bindings) name #f)
           (error "can't reference unbound variable" name)))
        (else
         (acond
           ((hash-table-ref/default (slot-value ns 'aliases) sym-ns #f)
            (lookup it sym))
           (else (error "can't reference unbound variable" name)))))))

  (defmethod (extend ns :Value/Ns sym value)
    (let ((name (name sym)))
      (hash-table-set! (slot-value ns 'bindings) name value)))

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
      (k (match (lookup env ast)
           (and (? Value/Box?) box) (slot-value box 'ref)
           v v))

      (? Value/Pair?)
      (match (name (first ast))
        'quote (k (second ast))
        'def   (evaluate (third ast) env
                         (lambda (v) (extend env (second ast) v) (k nil)))
        'do    (evaluate-block (rest ast) env k)
        'if    (evaluate-if ast env k)
        'fn    (k (Value/Closure (second ast) (third ast) env))
        _      (evaluate-call ast env k))))

  (def centring.core
    (Value/Ns 'centring.core
      (alist->hash-table
        `((< . ,(numeric-binop Value/Bool <))
          (* . ,(numeric-binop Value/Int *))
          (- . ,(numeric-binop Value/Int -))
          (writeln . ,(native-fn (v)
                        (writeln (denotate v))
                        nil))
          (*ns* . ,(Value/Box nil))))
      (make-hash-table)
      (make-hash-table)))

  (set! (slot-value (hash-table-ref (slot-value centring.core 'bindings) '*ns*)
                    'ref)
        centring.core))

;;;;

(import centring.bootstrap
        (only centring.interpreter evaluate analyze centring.core))
(use coops
     (only miscmacros let/cc)
     (only extras read-file)
     (only ports with-input-from-string))

(def (main args)
  (let ((expr (match args
                `("-e" ,estr) (with-input-from-string estr read)
                `(,filename) `(do ,@(read-file filename)))))
    (let/cc k
      (evaluate (analyze expr) centring.core k))))

(main (command-line-arguments))

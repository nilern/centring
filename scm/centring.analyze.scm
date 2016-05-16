(module centring.analyze
  *

  (import scheme chicken)
  (use matchable
       sequences
       coops
       (only data-structures compose)

       centring.schring
       (only centring.value ns-name ns name))

  (define ns-sep '/)

  ;;;;

  (define-enum AST
    (Fn formals types body)
    (Fix bindings body)
    (Primop op args)
    
    (Global ns name)
    (Const val))

  ;;;;

  (define (analyze sexp)
    (match sexp
      ((? symbol?)
       (call-with-values (lambda () (ns-name sexp)) Global))
      
      ((? literal?)
       (Const sexp))
      
      (((and (? special-form?) op) . args)
       (analyze-sf sexp))
      
      (((and (? intrinsic?) op) . args)
       (analyze-intr sexp))
      
      ((callee . args)
       (Primop 'call (apply vector (analyze callee) (fmap analyze args))))
      
      (_ (error "unable to analyze" sexp))))

  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn formals types body)
       (Fn (list->vector formals) (list->vector types) (analyze body)))
      (('letrec bindings body)
       (Fix (smap #() (match-lambda ((name val) `(,name . ,(analyze val))))
                  bindings)
            (analyze body)))))

  (define (analyze-intr sexp)
    (match-let (((op . args) sexp))
      ;(if (valid-intrinsic? (name op) args)
      (Primop (name op) (smap #() analyze args))))
      ;(error "invalid intrinsic" sexp))))

  (define (literal? v)
    (or (fixnum? v) (boolean? v)))

  (define (special-form? sym)
    (and (symbol? sym) (Some? (ns sym)) (eq? (unwrap (ns sym)) 'centring.sf)))

  (define (intrinsic? sym)
    (and (symbol? sym) (Some? (ns sym)) (eq? (unwrap (ns sym)) 'centring.intr)))

  ;;;;

  (define-generic (fold-ast f node))

  (define-method (fold-ast (f #t) (ast <Fn>))
    (f ast (fold-ast f (.body ast))))

  (define-method (fold-ast (f #t) (ast <Fix>))
    (f ast
       (fmap (compose (cute fold-ast f <>) cdr) (.bindings ast))
       (fold-ast f (.body ast))))

  (define-method (fold-ast (f #t) (ast <Primop>))
    (f ast (fmap (cute fold-ast f <>) (.args ast))))

  (define-method (fold-ast (f #t) (ast <Global>))
    (f ast))

  (define-method (fold-ast (f #t) (ast <Const>))
    (f ast))

  ;;;;

  (define-generic (ast->sexpr-rf ast))

  (define-method (ast->sexpr-rf (node <Fn>) br)
    `($fn ,(vector->list (.formals node)) ,(vector->list (.types node)) ,br))

  (define-method (ast->sexpr-rf (node <Fix>) vrs br)
    `($letrec ,(map (match-lambda* (((name . _) vr) `(,name ,vr)))
                    (vector->list (.bindings node))
                    (vector->list vrs))
              ,br))

  (define-method (ast->sexpr-rf (node <Primop>) br)
    `(,(symbol-append '% (.op node)) ,@(vector->list br)))

  (define-method (ast->sexpr-rf (node <Global>))
    (symbol-append (unwrap-or (.ns node) '@@) ns-sep (.name node)))

  (define-method (ast->sexpr-rf (node <Const>))
    (.val node))

  (define (ast->sexpr ast)
    (fold-ast ast->sexpr-rf ast)))

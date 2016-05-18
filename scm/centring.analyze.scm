(module centring.analyze
  *

  (import scheme chicken)
  (use matchable
       (only anaphora aif)
       sequences
       coops
       persistent-hash-map
       (only data-structures compose)

       centring.schring
       (only centring.value ns-name ns name))

  (define ns-sep '/)

  ;;;;

  (define-enum AST
    (Fn formals types body)
    (Fix bindings body)
    (Primop op args)

    (Splat val)
    (Global ns name)
    (Local name)
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
       (Fn (analyze-formals formals) (analyze-formals types) (analyze body)))
      (('letrec bindings body)
       (Fix (smap #() (match-lambda ((name val) `(,name . ,(analyze val))))
                  bindings)
            (analyze body)))
      (((? (cute eq? <> '...)) inner)
       (Splat (analyze inner)))
      (('quote val) ; TODO: check it is a valid Centring value
       (Const val))))

  (define (analyze-intr sexp)
    (match-let (((op . args) sexp))
      ;(if (valid-intrinsic? (name op) args)
      (Primop (name op) (smap #() analyze args))))
      ;(error "invalid intrinsic" sexp))))

  (define (analyze-formals formals)
    (define (analyze-formal formal)
      (match formal
        (((? (cute eq? <> '...)) f) (Splat f))
        (_ formal)))
    (smap #() analyze-formal formals))

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

  (define-method (fold-ast (f #t) (ast <Splat>))
    (f ast (fold-ast f (.val ast))))

  (define-method (fold-ast (f #t) (ast <Global>))
    (f ast))

  (define-method (fold-ast (f #t) (ast <Local>))
    (f ast))

  (define-method (fold-ast (f #t) (ast <Const>))
    (f ast))

  ;;;;

  (define-generic (ast->sexpr-rf ast))

  (define-method (ast->sexpr-rf (node <Fn>) br)
    (define formal->sexpr
      (match-lambda
        (($ Splat f) `($... ,f))
        (f f)))
    `($fn ,(smap '() formal->sexpr (.formals node))
          ,(smap '() formal->sexpr (.types node))
          ,br))

  (define-method (ast->sexpr-rf (node <Fix>) vrs br)
    `($letrec ,(map (match-lambda* (((name . _) vr) `(,name ,vr)))
                    (vector->list (.bindings node))
                    (vector->list vrs))
              ,br))

  (define-method (ast->sexpr-rf (node <Primop>) br)
    `(,(symbol-append '% (.op node)) ,@(vector->list br)))

  (define-method (ast->sexpr-rf (node <Global>))
    (symbol-append (unwrap-or (.ns node) '@@) ns-sep (.name node)))

  (define-method (ast->sexpr-rf (node <Splat>) ir)
    `($... ,ir))

  (define-method (ast->sexpr-rf (node <Local>))
    (.name node))

  (define-method (ast->sexpr-rf (node <Const>))
    (.val node))

  (define (ast->sexpr ast)
    (fold-ast ast->sexpr-rf ast))

  (define (map-formal f formal)
    (match formal
      (($ Splat name) (Splat (f name)))
      (name (f name))))

  ;;;; Alphatize & specialize

  ;;; TODO: ns-resolve globals (?)

  (define (alphatize&specialize ast)
    (define (alph&spec env node)
      (match node
        (($ Fn formals types body)
         (let* ((fnames (fmap formal-name formals))
                (env* (add-locals env fnames)))
           (Fn (fmap (cute replace-formal env* <>) formals) types
               (alph&spec env* body))))
        (($ Fix bindings body)
         (let* ((names (fmap car bindings))
                (vals (fmap cdr bindings))
                (env* (add-locals env names)))
           (Fix (fmap (match-lambda
                       ((name . val)
                        (cons (replace-sym env* name) (alph&spec env* val))))
                      bindings)
                (alph&spec env* body))))
        (($ Primop op args)
         (Primop op (fmap (cute alph&spec env <>) args)))
        (($ Splat val)
         (Splat (alph&spec env val)))
        ((? Global?) (replace-global env node))
        ((? Const?) node)))
    (alph&spec (persistent-map) ast))

  (define (add-locals env localnames)
    (let ((env* (map->transient-map env)))
      (doseq (name localnames)
        (map-add! env* name (gensym name)))
      (persist-map! env*)))

  (define (formal-name f)
    (match f
      (($ Splat f) f)
      (f f)))

  (define (replace-sym env sym)
    (map-ref env sym sym))

  (define (replace-formal env f)
    (map-formal (cute replace-sym env <>) f))

  (define (replace-global env g)
    (match g
      (($ Global (? Some?) _) g)
      (($ Global (? None?) name) (aif (map-ref env name #f) (Local it) g)))))
            
        
                
         

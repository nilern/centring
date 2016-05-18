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

  ;; HACK: The 'A'-prefix prevents coops breakage:
  (define-enum AST
    (AFn formals types body)
    (AFix bindings body)
    (APrimop op args)

    (ASplat val)
    (AGlobal ns name)
    (ALocal name)
    (AConst val))

  ;;;;

  (define (analyze sexp)
    (match sexp
      ((? symbol?)
       (call-with-values (lambda () (ns-name sexp)) AGlobal))
      
      ((? literal?)
       (AConst sexp))
      
      (((and (? special-form?) op) . args)
       (analyze-sf sexp))
      
      (((and (? intrinsic?) op) . args)
       (analyze-intr sexp))
      
      ((callee . args)
       (APrimop 'call (apply vector (analyze callee) (fmap analyze args))))
      
      (_ (error "unable to analyze" sexp))))

  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn formals types body)
       (AFn (analyze-formals formals) (analyze-formals types) (analyze body)))
      (('letrec bindings body)
       (AFix (smap #() (match-lambda ((name val) `(,name . ,(analyze val))))
                  bindings)
            (analyze body)))
      (((? (cute eq? <> '...)) inner)
       (ASplat (analyze inner)))
      (('quote val) ; TODO: check it is a valid Centring value
       (AConst val))))

  (define (analyze-intr sexp)
    (match-let (((op . args) sexp))
      ;(if (valid-intrinsic? (name op) args)
      (APrimop (name op) (smap #() analyze args))))
      ;(error "invalid intrinsic" sexp))))

  (define (analyze-formals formals)
    (define (analyze-formal formal)
      (match formal
        (((? (cute eq? <> '...)) f) (ASplat f))
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

  (define-method (fold-ast (f #t) (ast <AFn>))
    (f ast (fold-ast f (.body ast))))

  (define-method (fold-ast (f #t) (ast <AFix>))
    (f ast
       (fmap (compose (cute fold-ast f <>) cdr) (.bindings ast))
       (fold-ast f (.body ast))))

  (define-method (fold-ast (f #t) (ast <APrimop>))
    (f ast (fmap (cute fold-ast f <>) (.args ast))))

  (define-method (fold-ast (f #t) (ast <ASplat>))
    (f ast (fold-ast f (.val ast))))

  (define-method (fold-ast (f #t) (ast <AGlobal>))
    (f ast))

  (define-method (fold-ast (f #t) (ast <ALocal>))
    (f ast))

  (define-method (fold-ast (f #t) (ast <AConst>))
    (f ast))

  ;;;;

  (define-generic (ast->sexpr-rf ast))

  (define-method (ast->sexpr-rf (node <AFn>) br)
    (define formal->sexpr
      (match-lambda
        (($ ASplat f) `($... ,f))
        (f f)))
    `($fn ,(smap '() formal->sexpr (.formals node))
          ,(smap '() formal->sexpr (.types node))
          ,br))

  (define-method (ast->sexpr-rf (node <AFix>) vrs br)
    `($letrec ,(map (match-lambda* (((name . _) vr) `(,name ,vr)))
                    (vector->list (.bindings node))
                    (vector->list vrs))
              ,br))

  (define-method (ast->sexpr-rf (node <APrimop>) br)
    `(,(symbol-append '% (.op node)) ,@(vector->list br)))

  (define-method (ast->sexpr-rf (node <AGlobal>))
    (symbol-append (unwrap-or (.ns node) '@@) ns-sep (.name node)))

  (define-method (ast->sexpr-rf (node <ASplat>) ir)
    `($... ,ir))

  (define-method (ast->sexpr-rf (node <ALocal>))
    (.name node))

  (define-method (ast->sexpr-rf (node <AConst>))
    (.val node))

  (define (ast->sexpr ast)
    (fold-ast ast->sexpr-rf ast))

  (define (map-formal f formal)
    (match formal
      (($ ASplat name) (ASplat (f name)))
      (name (f name))))

  ;;;; Alphatize & specialize

  ;;; TODO: ns-resolve globals (?)

  (define (alphatize&specialize ast)
    (define (alph&spec env node)
      (match node
        (($ AFn formals types body)
         (let* ((fnames (fmap formal-name formals))
                (env* (add-locals env fnames)))
           (AFn (fmap (cute replace-formal env* <>) formals) types
                (alph&spec env* body))))
        (($ AFix bindings body)
         (let* ((names (fmap car bindings))
                (vals (fmap cdr bindings))
                (env* (add-locals env names)))
           (AFix (fmap (match-lambda
                        ((name . val)
                         (cons (replace-sym env* name) (alph&spec env* val))))
                       bindings)
                 (alph&spec env* body))))
        (($ APrimop op args)
         (APrimop op (fmap (cute alph&spec env <>) args)))
        (($ ASplat val)
         (ASplat (alph&spec env val)))
        ((? AGlobal?) (replace-global env node))
        ((? AConst?) node)))
    (alph&spec (persistent-map) ast))

  (define (add-locals env localnames)
    (let ((env* (map->transient-map env)))
      (doseq (name localnames)
        (map-add! env* name (gensym name)))
      (persist-map! env*)))

  (define (formal-name f)
    (match f
      (($ ASplat f) f)
      (f f)))

  (define (replace-sym env sym)
    (map-ref env sym sym))

  (define (replace-formal env f)
    (map-formal (cute replace-sym env <>) f))

  (define (replace-global env g)
    (match g
      (($ AGlobal (? Some?) _) g)
      (($ AGlobal (? None?) name) (aif (map-ref env name #f) (ALocal it) g)))))
            
        
                
         

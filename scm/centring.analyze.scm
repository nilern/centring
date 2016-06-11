(module centring.analyze
  (<const> <global>
   .val .res-ns .ns .name
   <do> <fn> <primop> <fix> <do>
   .arg .cases .op .args .bindings .body .stmts
   analyze ast->sexp alphatize&specialize)

  (import scheme chicken)
  (use matchable
       sequences
       coops
       persistent-hash-map
       (only miscmacros let/cc)
       (only anaphora aif awhen)
        
       (only centring.util ns name ns-name fmap-st doseq))

  ;;;; AST Classes

  (define-class <ast> ())

  (define-class <const> (<ast>)
    ((val accessor: .val)))
  (define-class <global> (<ast>)
    ((res-ns accessor: .res-ns initform: #f)
     (ns accessor: .ns)
     (name accessor: .name)))
  (define-class <local> (<ast>)
    ((name accessor: .name)))

  (define-class <fn> (<ast>)
    ((arg accessor: .arg)
     (cases accessor: .cases)))
  (define-class <primop> (<ast>)
    ((op accessor: .op)
     (args accessor: .args)))
  (define-class <fix> (<ast>)
    ((bindings accessor: .bindings)
     (body accessor: .body)))
  (define-class <do> (<ast>)
    ((stmts accessor: .stmts)))

  ;;;; Turn S-exprs into AST:s

  (define (analyze sexp)
    (match sexp
      ((? special-form?) (analyze-sf sexp))
      
      ((? intrinsic?) (analyze-intr sexp))
      
      ((? literal?) (make <const> 'val sexp))

      ((? symbol?) (analyze-id sexp))

      ((? pair?) (make <primop> 'op 'call 'args (smap #() analyze sexp)))

      (_ (error "unable to analyze" sexp))))

  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn arg . cases)
       (make <fn>
         'arg arg
         'cases (map (cute smap #() analyze <>) cases)))

      (('letrec bindings body)
       (make <fix>
         'bindings (smap #()
                         (match-lambda ((var val) (cons var (analyze val))))
                         bindings)
         'body (analyze body)))
      
      (('do . stmts)
       (make <do> 'stmts (smap #() analyze stmts)))
      
      (('quote (and v (or (? literal?) (? symbol?))))
       (make <const> 'val v))

      (_ (error "invalid special form" sexp))))

  (define (analyze-intr sexp)
    (make <primop>
      'op (name (car sexp))
      'args (smap #() analyze (cdr sexp))))

  (define (analyze-id id)
    (call-with-values (lambda () (ns-name id))
      (cute make <global> 'ns <> 'name <>)))

  (define (special-form? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'centring.sf)))

  (define (intrinsic? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'centring.intr)))

  (define (literal? v)
    (or (fixnum? v) (boolean? v) (keyword? v)))

  ;;;; Turn AST:s into S-exprs for Debugging

  (define-generic (ast->sexp ast))

  (define-method (ast->sexp (ast <const>))
    (let ((v (.val ast)))
      (if (and (symbol? v) (not (keyword? v)))
        `($quote ,v)
        v)))

  (define-method (ast->sexp (ast <global>))
    (symbol-append (or (.ns ast) '@@)
                   '/ (.name ast) #|'<= (or (.res-ns ast) '??)|#))

  (define-method (ast->sexp (ast <local>))
    (.name ast))

  (define-method (ast->sexp (ast <fn>))
    `($fn ,(.arg ast) ,@(map (cute smap '() ast->sexp <>) (.cases ast))))

  (define-method (ast->sexp (ast <primop>))
    `(,(symbol-append '% (.op ast))
      ,@(smap '() ast->sexp (.args ast))))

  (define-method (ast->sexp (ast <fix>))
    `($letrec ,(smap '()
                     (match-lambda ((var . val) (list var (ast->sexp val))))
                     (.bindings ast))
              ,(ast->sexp (.body ast))))

  (define-method (ast->sexp (ast <do>))
    `($do ,@(smap '() ast->sexp (.stmts ast))))

  ;;;; Alphatize & Specialize
  
  (define-record AEnv ns replacements)

  (define (add-local env localname)
    (make-AEnv (AEnv-ns env)
               (map-add (AEnv-replacements env) localname (gensym localname))))

  (define (add-locals env localnames)
    (let ((rpls* (map->transient-map (AEnv-replacements env))))
      (doseq (name localnames)
        (map-add! rpls* name (gensym name)))
      (make-AEnv (AEnv-ns env) (persist-map! rpls*))))

  (define (replace-sym env sym)
    (map-ref (AEnv-replacements env) sym))

  (define (alphatize&specialize init-ns ast)
    ;; FIXME: get rid of all this multiple return values -mucking
    (define-generic (alph&spec ast))

    (define-method (alph&spec (ast <const>) env)
      (values ast env))

    (define-method (alph&spec (ast <global>) env)
      (values
       (let/cc return
         (unless (.ns ast)
           (awhen (replace-sym env (.name ast))
             (return (make <local> 'name it))))
         (make <global> 'res-ns (AEnv-ns env) 'ns (.ns ast) 'name (.name ast)))
       env))

    (define-method (alph&spec (ast <fn>) env)
      (let* ((env* (add-local env (.arg ast))))
        (receive (cases* env**) (fmap-st (cute fmap-st alph&spec <> <>)
                                         (.cases ast) env*)
          (values
           (make <fn>
             'arg (replace-sym env* (.arg ast))
             'cases cases*)
           (make-AEnv (AEnv-ns env**) (AEnv-replacements env))))))

    (define-method (alph&spec (ast <primop>) env)
      ;; TODO: throw errors if set-ns! is found outside toplevel or
      ;; set-global! is used on a qualified var
      (let ((env* (if (eq? (.op ast) 'set-ns!)
                    (make-AEnv (.val (vector-ref (.args ast) 0))
                               (AEnv-replacements env))
                    env)))
        (receive (args* env**) (fmap-st alph&spec (.args ast) env*)
          (values (make <primop> 'op (.op ast) 'args args*) env**))))

    (define-method (alph&spec (ast <fix>) env)
      (let* ((vars (smap '() car (.bindings ast)))
             (vars* (map gensym vars))
             (env* (add-locals env vars)))   
        (receive (bindings* env**)
          (fmap-st (match-lambda*
                    (((var . val) env)
                     (receive (val* env*) (alph&spec val env)
                       (values
                        (cons (replace-sym env var) val*)
                              env*))))
                   (.bindings ast)
                   env*)
          (receive (body* env***) (alph&spec (.body ast) env**)
            (values
             (make <fix> 'bindings bindings* 'body body*)
             env***)))))

    (define-method (alph&spec (ast <do>) env)
      (receive (stmts* env*) (fmap-st alph&spec (.stmts ast) env)
        (values (make <do> 'stmts stmts*) env*)))
    
    (receive (ast* env) (alph&spec ast (make-AEnv init-ns (persistent-map)))
      ast*)))

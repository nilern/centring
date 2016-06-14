(module centring.analyze
  (<const> <global>
   .val .res-ns .ns .name
   <do> <fn> <primop> <fix> <do>
   .arg .cases .op .args .bindings .body .stmts
   analyze ast->sexp alphatize&specialize dnf-convert)

  (import scheme chicken)
  (use matchable
       sequences
       (srfi 42)
       vector-lib
       coops
       persistent-hash-map
       (only miscmacros let/cc)
       (only anaphora aif awhen)
       (only data-structures identity)
        
       (only centring.util ns name ns-name fmap-st doseq))

  ;;;; AST Classes

  ;; FIXME: OO -> pattern matching

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

  ;;;; Traversal

  ;; HACK: Theoretically these are questionable:
  (define-method (fmap (_ #t) (ast <const>)) ast)
  (define-method (fmap (_ #t) (ast <global>)) ast)
  (define-method (fmap (_ #t) (ast <local>)) ast)

  (define-method (fmap (f #t) (ast <fn>))
    (make <fn>
      'arg (.arg ast)
      'cases (map (smap #() f) (.cases ast))))
  
  (define-method (fmap (f #t) (ast <do>))
    (make <do> 'stmts (smap #() f (.stmts ast))))
  
  (define-method (fmap (f #t) (ast <fix>))
    (make <fix>
      'bindings (smap #() f (.bindings ast))
      'body (f (.body ast))))

  (define-method (fmap (f #t) (ast <primop>))
    (make <primop>
      'op (.op ast)
      'args (smap #() f (.args ast))))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

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
      ast*))

  ;;;; DNF conversion

  (define-generic (dnf-node node))

  (define-method (dnf-node (node #t))
    (make-or (vector (make-and (vector node)))))

  (define-method (dnf-node (node <primop>))
    (case (.op node)
      ((bior) ; convert children and concatenate:
       (or-ors (smap #() dnf-node (.args node))))
      ((band) ; convert children and distribute:
       (and-ors (smap #() dnf-node (.args node))))
      ((bnot) ; push `not` to leaves, reconvert result:
       (not-node (vector-ref (.args node) 0)))
      (else ; just wrap in `or` and `and`:
       (make-or (vector (make-and (vector node)))))))

  (define and-ors
    (let ((combine (lambda (a b)
                     (if a
                       (vector-ec (:vector l a) (:vector r b)
                                  (make-and (vector l r)))
                       b))))
      (match-lambda
       (#() (make-or (vector (make-and (vector (make <const> 'val #t))))))
       (#(a) a)
       (#(a b) (make-or (combine (.args a) (.args b))))
       ((and (? vector?) ors)
        (make-or (foldl combine #f (smap #() .args ors)))))))

  (define (or-ors ors)
    (make-or (foldl (lambda (acc a) (vector-append acc (.args a))) #() ors)))

  (define-generic (not-node a))
  (define-method (not-node (a #t))
    (make-or (vector (make-and (vector (make-not a))))))
  (define-method (not-node (a <primop>))
    (case (.op a)
      ((bior) (dnf-node (make-and (smap #() make-not (.args a)))))
      ((band) (dnf-node (make-or (smap #() make-not (.args a)))))
      ((bnot) (dnf-node (make-or (vector (dnf-node (vector-ref (.args a) 0))))))
      (else (make-or (vector (make-and (vector (make-not a))))))))

  (define make-or (cute make <primop> 'op 'bior 'args <>))
  (define make-and
    (let* ((build (cute make <primop> 'op 'band 'args <>))
           (clause (lambda (node)
                     (if (and-node? node) (.args node) (vector node))))
           (combine (lambda (a b)
                      (build (vector-append (clause a) (clause b))))))
      (match-lambda
       (#() (build (vector (make <const> 'val #f))))
       (#(a) (build (vector a)))
       (#(a b) (combine a b))
       ((and (? vector?) as) (build (foldl combine #() (smap #() .args as)))))))
  (define make-not (o (cute make <primop> 'op 'bnot 'args <>) vector))

  (define (and-node? node)
    (and (eq? (class-of node) <primop>) (eq? (.op node) 'band)))

  (define-generic (dnf-convert ast))

  (define-method (dnf-convert (ast #t))
    (fmap dnf-convert ast))
    
  (define-method (dnf-convert (ast <fn>))
    (make <fn>
      'arg (.arg ast)
      'cases (map (match-lambda
                   (#(cond body)
                    (vector (dnf-node cond) (dnf-convert body))))
                  (.cases ast)))))
    

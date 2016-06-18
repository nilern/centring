(module centring.analyze
  *

  (import scheme chicken)
  (use matchable
       sequences
       vector-lib
       persistent-hash-map
       (srfi 42)
       (only anaphora aif)

       centring.util
       centring.ast)
  
  ;;;; Turn S-exprs into AST:s

  (define (analyze sexp)
    (match sexp
      ((? special-form?) (analyze-sf sexp))
      
      ((? intrinsic?) (analyze-intr sexp))
      
      ((? literal?) (Const sexp (persistent-map)))

      ((? symbol?) (analyze-id sexp))

      ((callee . args)
       (Primop 'apply
               (vector (analyze callee)
                       (Primop 'rec
                               (mapv analyze (cons 'centring.lang/Tuple args))
                               (persistent-map)))
               (persistent-map)))

      (_ (error "unable to analyze" sexp))))
  
  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn arg . cases)
       (Fn arg (mapv (cute mapv analyze <>) cases) (persistent-map)))

      (('letrec bindings body)
       (Fix (mapv
             (match-lambda ((var val) (cons var (analyze val))))
             bindings)
            (analyze body)
            (persistent-map)))
      
      (('do . stmts)
       (Do (mapv analyze stmts) (persistent-map)))
      
      (('quote (and v (or (? literal?) (? symbol?))))
       (Const v (persistent-map)))

      (_ (error "invalid special form" sexp))))

  (define (analyze-intr sexp)
    (Primop (name (car sexp)) (mapv analyze (cdr sexp)) (persistent-map)))

  (define (analyze-id id)
    (call-with-values (lambda () (ns-name id))
      (cute Global #f <> <> (persistent-map))))

  ;;;; Turn AST:s into S-exprs

  (define (ast->sexp ast)
    (match ast
      (($ Fn arg cases _)
       `($fn ,arg ,@(smap '() (cute smap '() ast->sexp <>) cases)))
      (($ Primop op args _)
       `(,(symbol-append '% op) ,@(smap '() ast->sexp args)))
      (($ Fix bindings body _)
       `($letrec ,(smap '() (match-lambda
                             ((var . expr) (cons var (ast->sexp expr))))
                        bindings)
                 ,(ast->sexp body)))
      (($ Do stmts _)
       `($do ,@(smap '() ast->sexp stmts)))
      (($ Const (and (? symbol?) val) _) `(quote ,val))
      (($ Const val _) val)
      (($ Global _ ns name _) (symbol-append (or ns '@@) ns-sep name))
      (($ Local name _) name)
      (_ (error "unable to display as S-expr" ast))))

  ;;;; Alphatize & Specialize

  (define (alphatize&specialize curr-ns ast)
    ;; TODO: throw errors if set-ns! is found outside toplevel or
    ;; set-global! is used on a qualified var
    (define (add-local env localname)
      (map-add env localname (gensym localname)))

    (define (add-locals env localnames)
      (let ((rpls* (map->transient-map env)))
        (doseq (name localnames)
               (map-add! rpls* name (gensym name)))
        (persist-map! rpls*)))
  
    (define (alph&spec env ast)
      (match ast
       (($ Fn arg cases ann)
        (let ((env* (add-local env arg)))
          (Fn (map-ref env* arg)
              (mapv (cute mapv (cute alph&spec env* <>) <>) cases)
              ann)))
       ((and ($ Primop 'set-ns! #(($ Const (and (? symbol?) ns-name) _)) _)
             node)
        (set! curr-ns ns-name)
        node)
       (($ Primop op args ann)
        (Primop op (mapv (cute alph&spec env <>) args) ann))
       (($ Fix bindings body ann)
        (let ((env* (add-locals env (mapv car bindings))))
          (Fix
           (mapv (match-lambda
                  ((var . expr) (cons (map-ref env* var) (alph&spec env* expr))))
                 bindings)
           (alph&spec env* body)
           ann)))
       (($ Do stmts ann)
        (Do (mapv (cute alph&spec env <>) stmts) ann))
       ((and (? Const?) node) node)
       (($ Global _ #f name ann)
        (aif (map-ref env name #f)
             (Local it ann)
             (Global curr-ns #f name ann)))
       (($ Global _ ns name ann) (Global curr-ns ns name ann))
       (_ (error "unable to alphatize etc." ast))))
    (alph&spec (persistent-map) ast))

  ;;;; DNF conversion

  ;; DNF-convert a Fn case condition:
  (define (dnf ast)
    (define (wrap node)
      (Primop 'bior (vector (Primop 'band (vector node) (persistent-map)))
              (persistent-map)))
    
    (define (combine-dnfs-with f default subnodes ann)
      (match subnodes
        (#() default)
        (#(node) node)
        ((? vector?)
         (Primop 'bior
                 (foldl f (Primop-args (peek subnodes)) (pop subnodes))
                 ann))))
    
    (match ast
      (($ Primop 'bior args ann)
       ;; convert args and flatten the resulting `or` of `or`:s:
       (combine-dnfs-with (lambda (acc v) (vector-append acc (Primop-args v)))
                          (wrap (Const #f ann)) (mapv dnf args) ann))
      
      (($ Primop 'band args ann)
       ;; convert args and distribute `and` over them:
       (combine-dnfs-with
        (lambda (acc v)
          (vector-ec (:vector l acc) (:vector r (Primop-args v))
            (Primop 'band
                    (vector-append (Primop-args l) (Primop-args r))
                    (persistent-map))))
        (wrap (Const #t ann)) (mapv dnf args) ann))
      
      (($ Primop 'bnot #(arg) ann)
       ;; Use some Boolean algebra laws and reconvert:
       (match arg
         (($ Primop 'bior args ann) ; De Morgan
          (dnf (Primop 'band
                       (mapv (lambda (v) (Primop 'bnot (vector v) (persistent-map)))
                             args)
                       ann)))
         (($ Primop 'band args ann) ; De Morgan
          (dnf (Primop 'bior
                       (mapv (lambda (v) (Primop 'bnot (vector v) (persistent-map)))
                             args)
                       ann)))
         (($ Primop 'bnot #(arg) ann) ; double negation
          (dnf arg))
         (_ (wrap (Primop 'bnot (vector (dnf-convert arg)) ann)))))
      
      (_ (wrap (dnf-convert ast)))))

  ;; Traverse an AST and DNF-convert Fn case conditions:
  (define (dnf-convert ast)
    (match ast
      (($ Fn arg cases ann)
       (Fn arg
           (mapv (match-lambda
                  (#(cond body) (vector (dnf cond) (dnf-convert body))))
                 cases)
           ann))
      (_ (node-map dnf-convert ast)))))

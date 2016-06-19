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
                               #f
                               (persistent-map)))
               #f
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
    (Primop (name (car sexp))
            (mapv analyze (cdr sexp))
            #f
            (persistent-map)))

  (define (analyze-id id)
    (call-with-values (lambda () (ns-name id))
      (cute Global #f <> <> (persistent-map))))

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
       (($ Primop op args conts ann)
        (Primop op (mapv (cute alph&spec env <>) args) conts ann))
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
      (Primop 'bior
              (vector (Primop 'band (vector node) #f (persistent-map)))
              #f
              (persistent-map)))
    
    (define (combine-dnfs-with f default subnodes ann)
      (match subnodes
        (#() default)
        (#(node) node)
        ((? vector?)
         (Primop 'bior
                 (foldl f (Primop-args (peek subnodes)) (pop subnodes))
                 #f
                 ann))))
    
    (match ast
      (($ Primop 'bior args _ ann)
       ;; convert args and flatten the resulting `or` of `or`:s:
       (combine-dnfs-with (lambda (acc v) (vector-append acc (Primop-args v)))
                          (wrap (Const #f ann)) (mapv dnf args) ann))
      
      (($ Primop 'band args _ ann)
       ;; convert args and distribute `and` over them:
       (combine-dnfs-with
        (lambda (acc v)
          (vector-ec (:vector l acc) (:vector r (Primop-args v))
            (Primop 'band
                    (vector-append (Primop-args l) (Primop-args r))
                    #f
                    (persistent-map))))
        (wrap (Const #t ann)) (mapv dnf args) ann))
      
      (($ Primop 'bnot #(arg) _ ann)
       ;; Use some Boolean algebra laws and reconvert:
       (match arg
         (($ Primop 'bior args _ ann) ; De Morgan
          (dnf (Primop 'band
                       (mapv (lambda (v)
                               (Primop 'bnot (vector v) #f (persistent-map)))
                             args)
                       #f
                       ann)))
         (($ Primop 'band args _ ann) ; De Morgan
          (dnf (Primop 'bior
                       (mapv (lambda (v)
                               (Primop 'bnot (vector v) #f (persistent-map)))
                             args)
                       #f
                       ann)))
         (($ Primop 'bnot #(arg) _ ann) ; double negation
          (dnf arg))
         (_ (wrap (Primop 'bnot (vector (dnf-convert arg)) #f ann)))))
      
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

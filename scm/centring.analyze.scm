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
      
      ((? literal?) (Const sexp))

      ((? symbol?) (analyze-id sexp))

      ((callee . args)
       (Primop 'apply
               (vector (analyze callee)
                       (Primop 'rec
                               (mapv analyze (cons 'centring.lang/Tuple args))
                               #f))
               #f))

      (_ (error "unable to analyze" sexp))))
  
  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn arg . cases)
       (Fn arg (mapv (match-lambda
                      ((cond body) (cons (analyze cond) (analyze body))))
                     cases) #f))

      (('letrec bindings body)
       (Fix (mapv
             (match-lambda ((var val) (cons var (analyze val))))
             bindings)
            (analyze body)))
      
      (('do . stmts)
       (Do (mapv analyze stmts)))
      
      (('quote (and v (or (? literal?) (? symbol?))))
       (Const v))

      (_ (error "invalid special form" sexp))))

  (define (analyze-intr sexp)
    (Primop (name (car sexp))
            (mapv analyze (cdr sexp))
            #f))

  (define (analyze-id id)
    (call-with-values (lambda () (ns-name id))
      (cute Global #f <> <>)))

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
       (($ Fn arg cases #f)
        (let ((env* (add-local env arg)))
          (Fn (map-ref env* arg)
              (mapv (cute map-pair (cute alph&spec env* <>) <>) cases)
              #f)))
       ((and ($ Primop 'set-ns! #(($ Const (and (? symbol?) ns-name))))
             node)
        (set! curr-ns ns-name)
        node)
       (($ Primop op args conts)
        (Primop op (mapv (cute alph&spec env <>) args) conts))
       (($ Fix bindings body)
        (let ((env* (add-locals env (mapv car bindings))))
          (Fix
           (mapv (match-lambda
                  ((var . expr) (cons (map-ref env* var) (alph&spec env* expr))))
                 bindings)
           (alph&spec env* body))))
       (($ Do stmts)
        (Do (mapv (cute alph&spec env <>) stmts)))
       ((and (? Const?) node) node)
       (($ Global _ #f name)
        (aif (map-ref env name #f)
             (Local it)
             (Global curr-ns #f name)))
       (($ Global _ ns name) (Global curr-ns ns name))
       (_ (error "unable to alphatize etc." ast))))
    (alph&spec (persistent-map) ast)))

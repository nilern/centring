(module centring.analyze
  *

  (import scheme chicken)
  (use matchable
       sequences
       vector-lib
       persistent-hash-map
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

      ((? pair?) (Primop 'call (mapv analyze sexp) (persistent-map)))

      (_ (error "unable to analyze" sexp))))
  
  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn arg . cases)
       (Fn arg (map (cute mapv analyze <>) cases) (persistent-map)))

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

  ;;;; Turn AST:s into S-exprs

  (define (ast->sexp ast)
    (match ast
      (($ Fn arg cases _)
       `($fn ,arg ,@(map (cute smap '() ast->sexp <>) cases)))
      (($ Primop op args _)
       `(,(symbol-append '% op) ,@(smap '() ast->sexp args)))
      (($ Fix bindings body _)
       `($letrec ,(smap '() (match-lambda
                             ((var . expr) (cons var (ast->sexp expr))))
                        bindings)
                 ,(ast->sexp body)))
      (($ Do stmts _)
       `($do ,@(smap '() ast->sexp stmts)))
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
              (map (cute mapv (cute alph&spec env* <>) <>) cases)
              ann)))
       ((and ($ Primop 'set-ns! #(($ Ann ($ Const (and (? symbol?) ns-name)) _)) _)
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
    (alph&spec (persistent-map) ast)))

;;   ;;;; DNF conversion

;;   (define-generic (dnf-node node))

;;   (define-method (dnf-node (node #t))
;;     (make-or (vector (make-and (vector node)))))

;;   (define-method (dnf-node (node <primop>))
;;     (case (.op node)
;;       ((bior) ; convert children and concatenate:
;;        (or-ors (mapv dnf-node (.args node))))
;;       ((band) ; convert children and distribute:
;;        (and-ors (mapv dnf-node (.args node))))
;;       ((bnot) ; push `not` to leaves, reconvert result:
;;        (not-node (vector-ref (.args node) 0)))
;;       (else ; just wrap in `or` and `and`:
;;        (make-or (vector (make-and (vector node)))))))

;;   (define and-ors
;;     (let ((combine (lambda (a b)
;;                      (if a
;;                        (vector-ec (:vector l a) (:vector r b)
;;                                   (make-and (vector l r)))
;;                        b))))
;;       (match-lambda
;;        (#() (make-or (vector (make-and (vector (make <const> 'val #t))))))
;;        (#(a) a)
;;        (#(a b) (make-or (combine (.args a) (.args b))))
;;        ((and (? vector?) ors)
;;         (make-or (foldl combine #f (mapv .args ors)))))))

;;   (define (or-ors ors)
;;     (make-or (foldl (lambda (acc a) (vector-append acc (.args a))) #() ors)))

;;   (define-generic (not-node a))
;;   (define-method (not-node (a #t))
;;     (make-or (vector (make-and (vector (make-not a))))))
;;   (define-method (not-node (a <primop>))
;;     (case (.op a)
;;       ((bior) (dnf-node (make-and (mapv make-not (.args a)))))
;;       ((band) (dnf-node (make-or (mapv make-not (.args a)))))
;;       ((bnot) (dnf-node (make-or (vector (dnf-node (vector-ref (.args a) 0))))))
;;       (else (make-or (vector (make-and (vector (make-not a))))))))

;;   (define make-or (cute make <primop> 'op 'bior 'args <>))
;;   (define make-and
;;     (let* ((build (cute make <primop> 'op 'band 'args <>))
;;            (clause (lambda (node)
;;                      (if (and-node? node) (.args node) (vector node))))
;;            (combine (lambda (a b)
;;                       (build (vector-append (clause a) (clause b))))))
;;       (match-lambda
;;        (#() (build (vector (make <const> 'val #f))))
;;        (#(a) (build (vector a)))
;;        (#(a b) (combine a b))
;;        ((and (? vector?) as) (build (foldl combine #() (mapv .args as)))))))
;;   (define make-not (o (cute make <primop> 'op 'bnot 'args <>) vector))

;;   (define (and-node? node)
;;     (and (eq? (class-of node) <primop>) (eq? (.op node) 'band)))

;;   (define-generic (dnf-convert ast))

;;   (define-method (dnf-convert (ast #t))
;;     (fmap dnf-convert ast))
    
;;   (define-method (dnf-convert (ast <fn>))
;;     (make <fn>
;;       'arg (.arg ast)
;;       'cases (map (match-lambda
;;                    (#(cond body)
;;                     (vector (dnf-node cond) (dnf-convert body))))
;;                   (.cases ast)))))
    

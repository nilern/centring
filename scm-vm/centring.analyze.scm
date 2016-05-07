(module centring.analyze
  *
  
  (import scheme chicken)
  (use (only matchable match match-let match-lambda)
       (only (srfi 1) remove)
       (only (srfi 13) string-prefix? string-index)
       (only vector-lib vector-map)
       (only data-structures identity o)
       (only anaphora aif)

       (only centring.util mapv mapl ns-name ns name)
       (only centring.instructions valid-intrinsic?))
  
  ;;;; AST

  (define-record Block label formals types body)
  (define-record Fix defns body)
  (define-record If cond then else)
  (define-record Primop op args)
  (define-record App callee args)

  (define-record Splat fd)
  
  (define-record Local name)
  (define-record Global ns name)
  (define-record Label name)
  (define-record Const val)

  ;;;; Analyze Sexpr

  (define (analyze sexp)
    (match sexp
      (((and (? special-form?) op) . args)
       (analyze-sf sexp))
      (((and (? intrinsic?) op) . args)
       (analyze-intr sexp))
      ((op . args)
       (make-App (analyze op) (mapv analyze args)))
      ((? symbol?)
       (analyze-id sexp))
      ((? literal?)
       (analyze-lit sexp))
      (_ (error "unable to analyze" sexp))))

  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('letfn defns body)
       (make-Fix (mapv analyze-defn defns) (analyze body)))
      (('if cond then else)
       (make-If (analyze cond) (analyze then) (analyze else)))
      (('quote val)
       (analyze-lit val))
      (_ (error "invalid special form" sexp))))

  (define (analyze-intr sexp)
    (match-let (((op . args) sexp))
      (if (valid-intrinsic? (name op) args)
        (make-Primop name (mapv analyze args))
        (error "invalid intrinsic" sexp))))

  (define (analyze-id id)
    (call-with-values (lambda () (ns-name id)) make-Global))

  (define (analyze-lit lit)
    (match lit
      ((? literal?) (make-Const lit))
      (_ (error "invalid literal" lit))))

  (define (analyze-defn defn)
    (match-let (((label formals types body) defn))
      (make-Block label (analyze-formals formals) (analyze-formals types)
                  (analyze body))))

  (define (analyze-formals formals)
    (define (analyze-formal formal)
      (match formal
        ((... f) (make-Splat f))
        (_ formal)))
    (mapv analyze-formal formals))

  (define (special-form? sym)
    (and (symbol? sym) (eq? (ns sym) 'centring.sf)))

  (define (intrinsic? sym)
    (and (symbol? sym) (eq? (ns sym) 'centring.intr)))

  (define (literal? v)
    (or (fixnum? v) (boolean? v)))

  ;;;; Traversal

  (define (fmap f node)
    (match node
      (($ Block label formals types body)
       (make-Block label formals types (f body)))
      (($ Fix defns body)
       (make-Fix defns (f body)))
      (($ If cond then else)
       (make-If (f cond) (f then) (f else)))
      (($ Primop op args)
       (make-Primop op (vector-map (lambda (_ v) (f v)) args)))
      (($ App callee args)
       (make-App (f callee) (vector-map (lambda (_ v) (f v)) args)))

      (($ Splat arg)
       (make-Splat (f arg)))

      ((or (? Local?) (? Global?) (? Const?) (? Label?)) node)))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  ;;;; Convert Back to Sexpr (for Debugging)

  (define (ast->sexp ast)
    (match ast
      (($ Block label formals types body)
       `(,label ,(mapl ast->sexp formals) ,(mapl ast->sexp types)
                ,(ast->sexp body)))
      (($ Fix defns body)
       `($letfn ,(mapl ast->sexp defns)
                ,(ast->sexp body)))
      (($ If cond then else)
       `($if ,(ast->sexp cond) ,(ast->sexp then) ,(ast->sexp else)))
      (($ Primop op args)
       `(,(string->symbol (string-append "%" (symbol->string op)))
         ,@(mapl ast->sexp args)))
      (($ App callee args)
       `(,(ast->sexp callee)
         ,@(mapl ast->sexp args)))

      (($ Splat arg) `($... ,(ast->sexp arg)))

      (($ Local name) name)
      (($ Global #f name) (symbol-append '@@ '/ name))
      (($ Global ns name) (symbol-append ns '/ name))
      (($ Const val) val)
      (($ Label name) name)
      ((? symbol?) ast)
      (_ (error "unable to display" ast)))))

  ;;;; Alphatize and Separate Locals and Labels from Globals

  ;; TODO: module-resolve globals to retain lexical scoping

  ;; (define (alphatize&specialize replacements node)
  ;;   (define (alph&spec replacements node)
  ;;     (match node
  ;;       (($ Block label formals types body)
  ;;        (let* ((fnames (formal-names formals))
  ;;               (fnames* (map gensym fnames))
  ;;               (replacements* (append (map (lambda (v v*)
  ;;                                             (cons v (make-Local v*)))
  ;;                                           fnames fnames*)
  ;;                                      replacements)))
  ;;          (make-Block (replace-sym replacements label)
  ;;                      (replace-formals replacements* formals)
  ;;                      types
  ;;                      (alph&spec replacements* body))
                 
  ;; (define (replace-sym replacements sym)
  ;;   (aif (assq sym replacements) (cdr it) sym))
  
  ;; (define (replacement-entry name)
  ;;   (cons name (gensym name)))
  
  ;; (define (alph&spec-defn replacements defn)
  ;;   (match-let (((f formals types body) defn))
  ;;     (let ((replacements* (append (map replacement-entry formals)
  ;;                                  replacements)))
  ;;       `(,(replace-sym replacements f)
  ;;         ,(map (cute replace-sym replacements* <>) formals)
  ;;         ,types
  ;;         ,(alphatize&specialize replacements* body)))))

  ;; (define (alphatize&specialize replacements node)
  ;;   (match node
  ;;     (($ Fix defns body)
  ;;      (let* ((replacements* (append (map (o replacement-entry car) defns)
  ;;                                    replacements)))
  ;;        (make-Fix (map (cute alph&spec-defn replacements* <>) defns)
  ;;                  (alphatize&specialize replacements* body))))
  ;;     ((or (? If?) (? Def?) (? Primop?) (? App?))
  ;;      (fmap (cute alphatize&specialize replacements <>) node))
  ;;     (($ Var name)
  ;;      (aif (assq name replacements)
  ;;        (make-Local (cdr it))
  ;;        (make-Global name)))
  ;;     ((? Const?) node))))

(module centring.analyze
  *
  
  (import scheme chicken)
  (use (only matchable match match-let match-lambda)
       (only (srfi 1) remove)
       (only (srfi 13) string-prefix? string-index)
       (only vector-lib vector-map)
       (only data-structures identity o)
       (only anaphora aif)
       persistent-hash-map
       sequences

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
      (((? (cute eq? <> '...)) inner)
       (make-Splat (analyze inner)))
      (_ (error "invalid special form" sexp))))

  (define (analyze-intr sexp)
    (match-let (((op . args) sexp))
      (if (valid-intrinsic? (name op) args)
        (make-Primop (name op) (mapv analyze args))
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
        (((? (cute eq? <> '...)) f) (make-Splat f))
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
       (make-Fix (vector-map (lambda (_ defn) (f defn)) defns) (f body)))
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

  (define (map-formal f formal)
    (match formal
      (($ Splat name) (make-Splat (f name)))
      (name (f name))))

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
      (_ ast)))

  ;;;; Alphatize and Separate Locals and Labels from Globals

  ;; TODO: module-resolve globals to retain lexical scoping
  
  (define (alphatize&specialize node)
    (define (alph&spec replacements node)
      (match node
        (($ Block label formals types body)
         (let* ((fnames (formal-names formals))
                (fnames* (map gensym fnames))
                (replacements*
                 (extend-replacements replacements make-Local fnames fnames*)))
           (make-Block (replace-sym replacements label)
                       (smap #() (cute replace-formal replacements* <>) formals)
                       types
                       (alph&spec replacements* body))))
        (($ Fix defns body)
         (let* ((labels (mapl Block-label defns))
                (labels* (map gensym labels))
                (replacements*
                 (extend-replacements replacements make-Label labels labels*)))
           (fmap (cute alph&spec replacements* <>) node)))
        ((or (? If?) (? Primop?) (? App?) (? Splat?))
         (fmap (cute alph&spec replacements <>) node))
        ((? Global?) (replace-global replacements node))
        ((? Const?) node)))
    (alph&spec (persistent-map) node))

  (define (formal-names formals)
    (mapl (match-lambda (($ Splat f) f) (f f)) formals))

  (define (extend-replacements rpls ctor vs vs*)
    (let ((trpls (map->transient-map rpls)))
      (for-each (lambda (v v*) (map-add! trpls v (ctor v*))) vs vs*)
      (persist-map! trpls)))

  (define (replace-sym replacements sym)
    (match (map-ref replacements sym)
      (($ Local name) name)
      (($ Label name) name)
      (#f sym)))

  (define (replace-formal replacements formal)
    (map-formal (cute replace-sym replacements <>) formal))

  (define (replace-global replacements g)
    (match g
      (($ Global #f name) (map-ref replacements name g))
      (_ g))))

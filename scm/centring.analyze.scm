(module centring.analyze
  (analyze ast->sexp
   <const> <global> <do> <primop>
   .val .ns .name .stmts .op .args)

  (import scheme chicken)
  (use matchable
       sequences
       coops
       (only anaphora aif)
        
       (only centring.util ns name ns-name))

  ;;;;

  (define-class <ast> ())

  (define-class <const> (<ast>)
    ((val :accessor .val)))
  (define-class <global> (<ast>)
    ((res-ns :accessor .res-ns)
     (ns :accessor .ns)
     (name :accessor .name)))

  (define-class <do> (<ast>)
    ((stmts :accessor .stmts)))
  (define-class <primop> (<ast>)
    ((op :accessor .op)
     (args :accessor .args)))

  ;;;;

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
      (('do . stmts)
       (make <do> 'stmts (smap #() analyze stmts)))
      
      (('quote (and v (or (? literal?) (? symbol?))))
       (make <const> 'val v))

      (_ (error "ivalid special form" sexp))))

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

  ;;;;

  (define-generic (ast->sexp ast))

  (define-method (ast->sexp (ast <const>))
    (let ((v (.val ast)))
      (if (and (symbol? v) (not (keyword? v)))
        `($quote ,v)
        v)))

  (define-method (ast->sexp (ast <global>))
    (aif (.ns ast)
      (symbol-append it '/ (.name ast))
      (.name ast)))

  (define-method (ast->sexp (ast <do>))
    `($do ,@(smap '() ast->sexp (.stmts ast))))

  (define-method (ast->sexp (ast <primop>))
    `(,(symbol-append '% (.op ast))
      ,@(smap '() ast->sexp (.args ast)))))

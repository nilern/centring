(module centring.coreast
  *
  
  (import scheme chicken)
  (use (only matchable match match-let match-lambda)
       (only (srfi 1) remove)
       (only (srfi 13) string-prefix? string-index)
       (only data-structures identity o)
       (only anaphora aif))

  ;;;; Utils

  (define (update-nth n f ls)
    (letrec ((udn (lambda (n vs)
                    (if (zero? n)
                      (cons (f (car vs)) (cdr vs))
                      (cons (car vs) (udn (sub1 n) (cdr vs)))))))
      (udn n ls)))

  (define update-defnbody (cute update-nth 3 <> <>))
  
  ;;;; AST

  (define-record If cond then else)
  (define-record Fix defns body)
  (define-record Def name val)
  (define-record Primop op args)
  (define-record App callee args)

  (define-record Var name)
  (define-record Local name)
  (define-record Global name)
  (define-record Const val)

  ;;;; Analyze Sexpr

  (define (symbol-prefix? prefix sym)
    (string-prefix? prefix (symbol->string sym)))

  (define (special-form? sym)
    (and (symbol? sym) (symbol-prefix? "centring.sf/" sym)))

  (define (intrinsic? sym)
    (and (symbol? sym) (symbol-prefix? "centring.intr/" sym)))

  (define (name sym)
    (let ((symstr (symbol->string sym)))
      (string->symbol (substring symstr (add1 (string-index symstr #\/))))))

  (define (analyze sexp)
    (match sexp
      (((and (? special-form?) op) . args) (analyze-sf (name op) args))
      (((and (? intrinsic?) op) . args)    (analyze-intr (name op) args))
      ((op . args)                         (make-App (analyze op)
                                                     (map analyze args)))
      
      ((? symbol?)                   (make-Var sexp))
      ((or (? fixnum?) (? boolean?)) (make-Const sexp))
      
      (_ (error "invalid expression" sexp))))

  (define (analyze-sf name args)
    (match (cons name args)
      (('if cond then else) (make-If (analyze cond)
                                     (analyze then)
                                     (analyze else)))
      
      (('letfn defns body)
       (make-Fix (map (cute update-defnbody analyze <>) defns) (analyze body)))
      (('def name val) (make-Def name (analyze val)))

      (('quote val) (make-Const val))
      
      (form (error "invalid special form" form))))

  (define (analyze-intr name args)
    (if (primop? (length args) name)
      (make-Primop name (map analyze args))
      (error "invalid intrinsic" (cons name args))))

  (define (primop? n sym)
    (or (vararg-primop? sym)
        (case n
          ((0) (memq sym '(unbound)))
          ((1) (memq sym '(ineg inot)))
          ((2) (memq sym '(iadd isub imul idiv irem imod
                           iand ior ixor iash
                           get-nth-field set-type!)))
          ((3) (memq sym '(set-nth-field!)))
          (else #f))))

  (define (vararg-primop? sym)
    (memq sym '(record)))

  ;;;; Traversal

  (define (fmap f node)
    (match node
      (($ If cond then else) (make-If (f cond) (f then) (f else)))
      (($ Fix defns body) (make-Fix (map (cute update-defnbody f <>) defns)
                                    (f body)))
      (($ Def name expr) (make-Def name (f expr)))
      (($ Primop op args) (make-Primop op (map f args)))
      (($ App callee args) (make-App (f callee) (map f args)))
      ((or (? Var?) (? Local?) (? Global?) (? Const?)) node)))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

  ;;;; Convert Back to Sexpr (for Debugging)

  (define (core->sexp ast)
    (match ast
      (($ If cond then else) `($if ,(core->sexp cond)
                                   ,(core->sexp then)
                                   ,(core->sexp else)))
      
      (($ Fix defns body)
       `($letfn ,(map (cute update-defnbody core->sexp <>) defns)
                ,(core->sexp body)))
      (($ Def name val) `($def ,name ,(core->sexp val)))
      
      (($ Primop op args)
       `(,(string->symbol (string-append "%" (symbol->string op)))
         ,@(map core->sexp args)))
      (($ App callee args) `(,(core->sexp callee) ,@(map core->sexp args)))
      
      (($ Var name) name)
      (($ Local name) name)
      (($ Global name) (string->symbol (string-append "^" (symbol->string name))))
      (($ Const val) val)))

  ;;;; Alphatize and Separate Locals from Globals

  ;; TODO: module-resolve globals to retain lexical scoping
  
  (define (replace-sym replacements sym)
    (aif (assq sym replacements) (cdr it) sym))
  
  (define (replacement-entry name)
    (cons name (gensym name)))
  
  (define (alph&spec-defn replacements defn)
    (match-let (((f formals types body) defn))
      (let ((replacements* (append (map replacement-entry formals)
                                   replacements)))
        `(,(replace-sym replacements f)
          ,(map (cute replace-sym replacements* <>) formals)
          ,types
          ,(alphatize&specialize replacements* body)))))

  (define (alphatize&specialize replacements node)
    (match node
      (($ Fix defns body)
       (let* ((replacements* (append (map (o replacement-entry car) defns)
                                     replacements)))
         (make-Fix (map (cute alph&spec-defn replacements* <>) defns)
                   (alphatize&specialize replacements* body))))
      ((or (? If?) (? Def?) (? Primop?) (? App?))
       (fmap (cute alphatize&specialize replacements <>) node))
      (($ Var name)
       (aif (assq name replacements)
         (make-Local (cdr it))
         (make-Global name)))
      ((? Const?) node))))
            

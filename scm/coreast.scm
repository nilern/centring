(module centring.coreast
  *
  
  (import scheme chicken)
  (use (only matchable match match-lambda)
       (only (srfi 13) string-prefix? string-index))

  ;;;; Utils

  (define (update-nth n f ls)
    (letrec ((udn (lambda (n vs)
                    (if (zero? n)
                      (cons (f (car vs)) (cdr vs))
                      (cons (car vs) (udn (sub1 n) (cdr vs)))))))
      (udn n ls)))

  (define update-defnbody (cute update-nth 3 <> <>))
  
  ;;;; AST

  (define-record Def name val)
  (define-record If cond then else)
  (define-record Do stmts)
  (define-record Fn formals types body)
  (define-record Fix defns body)
  (define-record Primop op args)
  (define-record App callee args)
  (define-record Halt val)

  (define-record Var name)
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
      (('do . stmts) (make-Do (map analyze stmts)))
      
      (('fn formals types body) (make-Fn formals types
                                         (analyze body)))
      (('letfn defns body)
       (make-Fix (map (cute update-defnbody analyze <>) defns) (analyze body)))
      (('def name val) (make-Def name (analyze val)))

      (('quote val) (make-Const val))
      (('halt val) (make-Halt (analyze val)))
      
      (form (error "invalid special form" form))))

  (define (analyze-intr name args)
    (match (cons name args)
      (((or 'add 'sub 'mul 'div) a b)
       (make-Primop name `(,(analyze a) ,(analyze b))))
      
      (form (error "invalid intrinsic" form))))

  ;;;; Convert Back to Sexpr (for Debugging)

  (define (core->sexp ast)
    (match ast
      (($ If cond then else) `($if ,(core->sexp cond)
                                   ,(core->sexp then)
                                   ,(core->sexp else)))
      (($ Do stmts) `($do ,@(map core->sexp stmts)))
      
      (($ Fn formals types body) `($fn ,formals ,types
                                       ,(core->sexp body)))
      (($ Fix defns body)
       `($letfn ,(map (cute update-defnbody core->sexp <>) defns)
                ,(core->sexp body)))
      (($ Def name val) `($def ,name ,(core->sexp val)))
      
      (($ Primop op args)
       `(,(string->symbol (string-append "%" (symbol->string op)))
         ,@(map core->sexp args)))
      (($ App callee args) `(,(core->sexp callee) ,@(map core->sexp args)))

      (($ Halt val) `($halt ,(core->sexp val)))
      
      (($ Var name) name)
      (($ Const val) val))))

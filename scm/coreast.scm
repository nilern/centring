(module centring.coreast
  *
  
  (import scheme chicken)
  (use (only matchable match)
       (only (srfi 13) string-prefix? string-index))
  
  ;;;; AST

  (define-record If cond then else)
  (define-record Fn formals types body)
  (define-record Primop op args)
  (define-record App callee args)

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
      ((op . args)
       (match op
         ((? special-form?) (analyze-sf (name op) args))
         ((? intrinsic?) (analyze-intr (name op) args))
         (_ (make-App (analyze op) (map analyze args)))))
      
      ((? symbol?) (make-Var sexp))
      ((or (? fixnum?) (? boolean?)) (make-Const sexp))
      (_ (error "invalid expression" sexp))))

  (define (analyze-sf name args)
    (match (cons name args)
      (('if cond then else) (make-If (analyze cond)
                                     (analyze then)
                                     (analyze else)))
      (('fn formals types body) (make-Fn formals types
                                         (analyze body)))
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
      (($ Fn formals types body) `($fn ,formals ,types
                                       ,(core->sexp body)))
      (($ Primop op args)
       `(,(string->symbol (string-append "%" (symbol->string op)))
         ,@(map core->sexp args)))
      (($ App callee args) `(,(core->sexp callee) ,@(map core->sexp args)))
      (($ Var name) name)
      (($ Const val) val))))

(module centring:interpreter
  (analyze

   denotate

   <Environment>
   <Interpreter>
   interpret)

  (import scheme chicken)

  (use coops
       coops-primitive-objects
       (only matchable match)
       (only anaphora aif acond)
       (only extras sprintf)
       (only (srfi 13) string-index)
       (only (srfi 69) make-hash-table hash-table-set! hash-table-ref/default))

  ;;;; Constants

  (define module-separator #\/)

  ;;;; Value

  ;; ;; Metacircular version would be:
  ;; (defenum Value
  ;;   (Int val)
  ;;   (Bool val)
  ;;   EmptyList
  ;;   Nothing
  ;;   (Symbol module name)
  ;;   (Pair left right)
  ;;   (Closure formals body env))

  (define-class <Value>)

  (define-class <Int> (<Value>)
    (val))

  (define-class <Bool> (<Value>)
    (val))

  (define-class <EmptyList> (<Value>))
  (define empty-list (make <EmptyList>))

  (define-class <Nothing> (<Value>))
  (define nothing (make <Nothing>))

  (define-class <Symbol> (<Value>)
    (module name))

  (define-class <Pair> (<Value>)
    (left
     right))

  (define-class <Closure> (<Value>)
    (formals
     body
     env))

  ;;;; Analyze

  (define-generic (analyze sexpr))

  (define-method (analyze (n <fixnum>))
    (make <Int> 'val n))

  (define-method (analyze (b <boolean>))
    (make <Bool> 'val b))

  (define-method (analyze (sym <symbol>))
    (let* ((sstr (symbol->string sym))
           (sepi (string-index sstr module-separator)))
      (if (or (not sepi)                     ; just a name
              (zero? sepi)                   ; starts with modsep
              (= sepi (string-length sstr))) ; ends with modsep
        (make <Symbol> 'module #f 'name (symbol->string sym))
        (make <Symbol>
          'module (substring sstr 0 sepi)
          'name   (substring sstr (add1 sepi))))))

  (define-method (analyze (b <null>))
    empty-list)

  (define-method (analyze (p <pair>))
    (make <Pair> 'left (analyze (car p)) 'right (analyze (cdr p))))

  ;;;; Denotate

  (define-generic (denotate val))

  (define-method (denotate (val #t))
    val)

  (define-method (denotate (n <Int>))
    (slot-value n 'val))

  (define-method (denotate (b <Bool>))
    (slot-value b 'val))

  (define-method (denotate (sym <Symbol>))
    (let ((modname (slot-value sym 'module)))
      (if modname
        (string->symbol
          (sprintf "~A/~A" (slot-value sym 'module) (slot-value sym 'name)))
        (string->symbol (slot-value sym 'name)))))

  (define-method (denotate (b <EmptyList>))
    '())

  (define-method (denotate (p <Pair>))
    `(,(denotate (slot-value p 'left)) . ,(denotate (slot-value p 'right))))

  ;;;; Interpreter Types and Operations

  (define-class <Environment> ()
    (table
     parent))

  (define-class <Interpreter> ()
    (module-registry))

  (define-generic (lookup env name))
  (define-generic (lookup-module itp name))
  (define-generic (extend env name val))

  (define-method (lookup (env <Environment>) name)
    (acond
      ((hash-table-ref/default (slot-value env 'table) name #f) it)
      ((slot-value env 'parent) (lookup it name))
      (else (error "can't reference unbound variable" name))))

  (define-method (lookup-module (itp <Interpreter>) name)
    (aif (hash-table-ref/default (slot-value itp 'module-registry) name #f)
      it
      (error "no such module" name)))

  (define-method (extend (env <Environment>) name val)
    (hash-table-set! (slot-value env 'table) name val)
    env)

  ;;;; Interpret

  (define-generic (interpret itp expr env))
  (define-generic (interpret-args itp args env))
  (define-generic (interpret-call itp callee args))

  (define-method (interpret (itp <Interpreter>) (expr #t) env)
    expr)

  (define-method (interpret (itp <Interpreter>) (expr <Symbol>) env)
    (match (slot-value expr 'module)
      (#f      (lookup env (slot-value expr 'name)))
      (modname (lookup (lookup-module itp modname) (slot-value expr 'name)))))

  (define (interpret-def itp expr env)
    (let* ((def-args (slot-value expr 'right))
           (name (slot-value (slot-value def-args 'left) 'name))
           (val (slot-value (slot-value def-args 'right) 'left)))
      (extend env name (interpret itp val env))))

  (define (interpret-stmts itp stmts env)
    (cond
      ((eq? stmts empty-list) nothing)
      ((eq? (slot-value stmts 'right) empty-list)
       (interpret itp (slot-value stmts 'left) env))
      (else (begin
              (interpret itp (slot-value stmts 'left) env)
              (interpret-stmts itp (slot-value stmts 'right) env)))))

  (define (interpret-conditional itp expr env)
    (let* ((args (slot-value expr 'right))
           (condition (slot-value args 'left))
           (then (slot-value (slot-value args 'right) 'left))
           (else (slot-value
                   (slot-value (slot-value args 'right) 'right)
                   'left))
           (cond-val (interpret itp condition env)))
      (if (not (eq? (class-of cond-val) <Bool>))
        (error "not a boolean" cond-val)
        (if (slot-value cond-val 'val)
          (interpret itp then env)
          (interpret itp else env)))))

  (define (interpret-fn itp expr env)
    (let* ((fn-args (slot-value expr 'right))
           (formals (slot-value fn-args 'left))
           (body (slot-value (slot-value fn-args 'right) 'left)))
      (make-closure formals body env)))

  (define (make-closure formals body env)
      (make <Closure> 'formals formals 'body body 'env env))

  (define-method (interpret-args (itp <Interpreter>) (args <EmptyList>) env)
    args)

  (define-method (interpret-args (itp <Interpreter>) (args <Pair>) env)
    (make <Pair>
      'left (interpret itp (slot-value args 'left) env)
      'right (interpret-args itp (slot-value args 'right) env)))

  ;; Args should already be evaluated and env built:
  (define-generic (bind-args formals args env))

  (define-method (bind-args (formals <EmptyList>) (args <EmptyList>) env)
    env)

  (define-method (bind-args (formals <Pair>) (args <Pair>) env)
    (extend env (slot-value (slot-value formals 'left) 'name)
                (slot-value args 'left))
    (bind-args (slot-value formals 'right) (slot-value args 'right) env))

  (define-method (bind-args (formals <Pair>) (args <EmptyList>) env)
    (error "too few arguments!"))

  (define-method (bind-args (formals <EmptyList>) (args #t) env)
    (error "too many arguments!"))

  (define-method (bind-args (formals <Symbol>) (args #t) env)
    (extend env (slot-value formals 'name) args))

  (define-method (interpret-call (itp <Interpreter>) (fn <Closure>) args)
    (let ((formals (slot-value fn 'formals))
          (body (slot-value fn 'body))
          (env (make <Environment>
                  'table (make-hash-table)
                  'parent (slot-value fn 'env))))
      (bind-args formals args env)
      (interpret itp body env)))

  (define-method (interpret (itp <Interpreter>) (expr <Pair>) env)
    (let ((op (slot-value expr 'left)))
      (if (and (eq? (class-of op) <Symbol>)
               (equal? (slot-value op 'module) "centring.ct"))
        (match (slot-value op 'name)
          ("def"   (interpret-def itp expr env))
          ("do"    (interpret-stmts itp (slot-value expr 'right) env))
          ("if"    (interpret-conditional itp expr env))
          ("quote" (slot-value (slot-value expr 'right) 'left))
          ("fn"    (interpret-fn itp expr env))
          (_       (error "no such special form" (slot-value op 'name))))
        (interpret-call itp (interpret itp op env)
                            (interpret-args itp (slot-value expr 'right) env))))))

  ;;;; Main

(import centring:interpreter)
(use (only coops make)
     (only matchable match)
     (only extras read-file printf)
     (only ports with-input-from-string)
     (only (srfi 69) make-hash-table))

(define (main arglist)
  (let ((expr (match arglist
                (`("-e" ,estr) (with-input-from-string estr read))
                (`(,filename) `(do ,@(read-file filename))))))
    (printf "~S~N"
      (denotate (interpret
                  (make <Interpreter> 'module-registry (make-hash-table))
                  (analyze expr)
                  (make <Environment> 'table (make-hash-table) 'parent #f))))))

(main (command-line-arguments))

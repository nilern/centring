(module centring:interpreter
  (analyze

   denotate

   <Int>
   <Bool>
   nothing
   <NativeFn>
   native-fn

   <Module>
   <Interpreter>
   interpret)

  (import scheme chicken)

  (use coops
       coops-primitive-objects
       (only matchable match)
       (only anaphora aif acond)
       (only miscmacros define-syntax-rule)
       (only extras printf sprintf)
       (only (srfi 13) string-index)
       (only (srfi 69) make-hash-table hash-table-set! hash-table-ref/default))

  ;;;; Utils

  (define push cons)
  (define peek car)

  ;;;; Constants

  (define module-separator #\/)

  ;;;; Value

  ;; ;; Metacircular version would be:
  ;; (defenum Value
  ;;   (Int val)
  ;;   (Bool val)
  ;;   (EmptyList)
  ;;   (Nothing)
  ;;   (Symbol module name)
  ;;   (Pair left right)
  ;;   (Macro expander)
  ;;   (Closure formals body env)
  ;;   (NativeFn fn)
  ;;   (Module name bindings aliases refers))

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

  (define-class <NativeFn> (<Value>)
    (fn))

  (define-class <Macro> (<Value>)
    (expander)) ;; Closure

  (define-class <Module> (<Value>)
    (name
     bindings
     aliases  ;; hash-table<string, Module>
     refers)) ;; hash-table<string, Symbol>

  ;;;; Sugar

  (define-syntax-rule (native-fn formals body ...)
    (make <NativeFn>
      'fn (lambda formals
            body ...)))

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
  (define-generic (denotate-list val))

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

  (define-method (denotate-list (p <Pair>))
    `(,(slot-value p 'left) . ,(denotate-list (slot-value p 'right))))

  (define-method (denotate-list (p <EmptyList>))
    '())

  ;;;; Interpreter Types and Operations

  (define-class <Environment> ()
    (bindings
     parent))

  (define-class <Interpreter> ()
    (envstack
     module-registry))

  (define-generic (lookup itp env name))
  (define-generic (lookup-module itp name))
  (define-generic (extend env name val))

  (define-method (lookup (itp <Interpreter>) (env <Environment>) (sym <Symbol>))
    (acond
      ((and (not (slot-value sym 'module))
            (hash-table-ref/default (slot-value env 'bindings) 
                                    (slot-value sym 'name)
                                    #f))
       it)
      ((slot-value env 'parent) (lookup itp it sym))
      (else (error "can't reference unbound variable" (denotate sym)))))

  (define-method (lookup (itp <Interpreter>) (mod <Module>) (sym <Symbol>))
    (let ((symname (slot-value sym 'name))
          (symmod (slot-value sym 'module)))
      (acond
        ;; Not module-qualified or explicitly qualified to this Module:
        ((or (not symmod) (equal? symmod (slot-value mod 'name)))
         (acond
           ;; Defined in this Module:
           ((hash-table-ref/default (slot-value mod 'bindings) symname #f) it)
           ;; Referred into this Module:
           ((hash-table-ref/default (slot-value mod 'refers) symname #f)
            (lookup itp mod it))
           (else (error "can't reference unbound variable" (denotate sym)))))
        ;; A Module-alias:
        ((hash-table-ref/default (slot-value mod 'aliases) symmod #f)
         (lookup itp it sym))
        ;; Some module that is 'completely unrelated' to this one:
        (else (lookup itp (lookup-module itp symmod) sym)))))

  (define-method (lookup-module (itp <Interpreter>) name)
    (aif (hash-table-ref/default (slot-value itp 'module-registry) name #f)
      it
      (error "no such module" name)))

  (define-method (extend (env <Environment>) name val)
    (hash-table-set! (slot-value env 'bindings) name val)
    env)

  (define-method (extend (mod <Module>) name val)
    (hash-table-set! (slot-value mod 'bindings) name val)
    mod)

  ;;;; Interpret

  (define-generic (interpret itp expr))
  (define-generic (interpret-args itp args))
  (define-generic (interpret-call itp callee args))

  (define-method (interpret (itp <Interpreter>) (expr #t))
    expr)

  (define-method (interpret (itp <Interpreter>) (expr <Symbol>))
    (lookup itp (peek (slot-value itp 'envstack)) expr))

  (define (interpret-def itp expr)
    (let* ((env (peek (slot-value itp 'envstack)))
           (def-args (slot-value expr 'right))
           (name (slot-value (slot-value def-args 'left) 'name))
           (val (slot-value (slot-value def-args 'right) 'left)))
      (extend env name (interpret itp val))))

  (define (interpret-stmts itp stmts)
    (cond
      ((eq? stmts empty-list) nothing)
      ((eq? (slot-value stmts 'right) empty-list)
       (interpret itp (slot-value stmts 'left)))
      (else (begin
              (interpret itp (slot-value stmts 'left))
              (interpret-stmts itp (slot-value stmts 'right))))))

  (define (interpret-conditional itp expr)
    (let* ((args (slot-value expr 'right))
           (condition (slot-value args 'left))
           (then (slot-value (slot-value args 'right) 'left))
           (else (slot-value
                   (slot-value (slot-value args 'right) 'right)
                   'left))
           (cond-val (interpret itp condition)))
      (if (not (eq? (class-of cond-val) <Bool>))
        (error "not a boolean" cond-val)
        (if (slot-value cond-val 'val)
          (interpret itp then)
          (interpret itp else)))))

  (define (interpret-fn itp expr)
    (let* ((env (peek (slot-value itp 'envstack)))
           (fn-args (slot-value expr 'right))
           (formals (slot-value fn-args 'left))
           (body (slot-value (slot-value fn-args 'right) 'left)))
      (make <Closure> 'formals formals 'body body 'env env)))

  (define-method (interpret-args (itp <Interpreter>) (args <EmptyList>))
    args)

  (define-method (interpret-args (itp <Interpreter>) (args <Pair>))
    (make <Pair>
      'left (interpret itp (slot-value args 'left))
      'right (interpret-args itp (slot-value args 'right))))

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

  (define-method (interpret-call (itp <Interpreter>) (fn <NativeFn>) args)
    (let ((f (slot-value fn 'fn)))
      (apply f (denotate-list (interpret-args itp args)))))

  (define-method (interpret-call (itp <Interpreter>) (fn <Closure>) args)
    (let ((formals (slot-value fn 'formals))
          (body (slot-value fn 'body))
          (env (make <Environment>
                  'bindings (make-hash-table)
                  'parent (slot-value fn 'env))))
      (bind-args formals (interpret-args itp args) env)
      (interpret (make <Interpreter>
                   'envstack (push env (slot-value itp 'envstack))
                   'module-registry (slot-value itp 'module-registry))
                 body)))

  ;; TODO: hygiene, separate macroexpansion phase (not intertwined with interpret)
  (define-method (interpret-call (itp <Interpreter>) (mac <Macro>) args)
    (let* ((expander (slot-value mac 'expander))
           (formals (slot-value expander 'formals))
           (body (slot-value expander 'body))
           (env (make <Environment>
                  'bindings (make-hash-table)
                  'parent (slot-value expander 'env))))
      (bind-args formals args env)
      (interpret itp
                 (interpret
                   (make <Interpreter>
                     'envstack (push env (slot-value itp 'envstack))
                     'module-registry (slot-value itp 'module-registry))
                   body))))

  (define-method (interpret (itp <Interpreter>) (expr <Pair>))
    (let ((op (slot-value expr 'left)))
      (if (and (eq? (class-of op) <Symbol>)
               (equal? (slot-value op 'module) "centring.ct"))
        (match (slot-value op 'name)
          ("def"   (interpret-def itp expr))
          ("do"    (interpret-stmts itp (slot-value expr 'right)))
          ("if"    (interpret-conditional itp expr))
          ("quote" (slot-value (slot-value expr 'right) 'left))
          ("fn"    (interpret-fn itp expr))
          ("macro" (make <Macro>
                      'expander
                      (interpret-fn
                        itp
                        (make <Pair>
                          'left (make <Symbol> 'module "centring.ct" 'name "fn")
                          'right (slot-value expr 'right)))))
          (_       (error "no such special form" (slot-value op 'name))))
        (interpret-call itp (interpret itp op) (slot-value expr 'right))))))

;;;; Main

(import centring:interpreter)
(use (only coops make slot-value)
     (only matchable match)
     (only extras read-file printf)
     (only ports with-input-from-string)
     (only (srfi 69) make-hash-table alist->hash-table))

(define (main arglist)
  (let ((expr (match arglist
                (`("-e" ,estr) (with-input-from-string estr read))
                (`(,filename) `(centring.ct/do ,@(read-file filename))))))
    (denotate (interpret
                (make <Interpreter>
                  'envstack
                  (list
                    (make <Module>
                      'name "centring.user"
                      'bindings (make-hash-table)
                      'aliases (make-hash-table)
                      'refers
                      (alist->hash-table
                        `(("writeln" . ,(analyze 'centring.core/writeln))
                          ("<" . ,(analyze 'centring.core/<))
                          ("*" . ,(analyze 'centring.core/*))
                          ("-" . ,(analyze 'centring.core/-))))))
                  'module-registry
                  (alist->hash-table
                    `(("centring.core" .
                       ,(make <Module>
                          'name "centring.core"
                          'bindings
                          (alist->hash-table
                            `(("writeln" . ,(native-fn (v)
                                              (printf "~S~N" (denotate v))
                                              nothing))
                              ("<" . ,(native-fn (a b)
                                        (make <Bool>
                                          'val (< (slot-value a 'val)
                                                  (slot-value b 'val)))))
                              ("*" . ,(native-fn (a b)
                                        (make <Int>
                                          'val (* (slot-value a 'val)
                                                  (slot-value b 'val)))))
                              ("-" . ,(native-fn (a b)
                                        (make <Int>
                                          'val (- (slot-value a 'val)
                                                  (slot-value b 'val)))))))
                          'aliases (make-hash-table)
                          'refers (make-hash-table))))))
                (analyze expr)))))

(main (command-line-arguments))

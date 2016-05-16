(module centring.expand
  *

  (import scheme chicken)
  (use matchable
       (only data-structures compose))

  (define keyword->symbol (compose string->symbol keyword->string))

  (define (analyze-formals formals)
    (match formals
      ('() (values '() '()))
           
      (((... ('= type)))
       (values `((... ,(gensym '_))) `((... (= ,type)))))
      (((... name))
       (values `((... ,name)) `((... centring.lang/Any))))
      (((... name (and (? keyword?) type)))
       (values `((... ,name)) `((... ,(keyword->symbol type)))))

      (`((= ,type) . ,rfs)
       (receive (names types) (analyze-formals rfs)
         (values (cons (gensym '_) names) (cons `(= ,type) types))))
      (`(,(and (? symbol?) name) ,(and (? keyword?) type) . ,rfs)
       (receive (names types) (analyze-formals rfs)
         (values (cons name names) (cons (keyword->symbol type) types))))
      (`(,(and (? symbol?) name) . ,rfs)
       (receive (names types) (analyze-formals rfs)
         (values (cons name names) (cons 'centring.lang/Any types))))

      (_ (error "invalid formals" formals))))
  
  (define (ctr-expand-1 sexp)
    (match sexp
      (('let ((name val) . nvs) . body) `((fn (,name) (let ,nvs ,@body)) ,val))
      (('let '() . body) `(do ,@body))
      
      (('do) '(centring.intr/record centring.lang/Tuple))
      (('do stmt) stmt)
      (('do stmt . stmts) `(let ((,(gensym '_) ,stmt)) ,@stmts))

      ;; These are backed up by special forms:
      (('fn formals body)
       (receive (names types) (analyze-formals formals)
         `(centring.sf/fn ,names ,types ,body)))
      (('fn formals . body)
       `(fn ,formals (do ,@body)))
      (('letfn defns body)
       `(centring.sf/letrec
         ,(map (match-lambda
                (((name . formals) . body) `(,name (fn ,formals ,@body))))
               defns)
         ,body))
      (('letfn defns . body)
       `(letfn ,defns (do ,@body)))
      (('quote val)
       `(centring.sf/quote ,val))

      ;; And these by intrinsics:
      (('if cond then else)
       `(centring.intr/brf ,cond (fn () ,then) (fn () ,else)))
      (('def (name . formals) body)
       `(def ,name (fn ,formals ,body)))
      (('def name val)
       `(centring.intr/set-global! (quote ,name) ,val))
           
      (_ sexp)))     

  (define (ctr-expand expr)
    (let ((expansion (ctr-expand-1 expr)))
      (if (eq? expansion expr)
        expr
        (ctr-expand expansion))))

  ;; Does not handle macro shadowing (i.e. (let ((if (fn (n) ...))) (if ...)))
  (define (expand-all expr)
    (let ((expansion (ctr-expand expr)))
      (if (list? expansion)
        (map expand-all (ctr-expand expr))
        expansion))))

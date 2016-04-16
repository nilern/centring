(module centring.expand
  *

  (import scheme chicken)
  (use (only matchable match)
       (only data-structures o))
  
  (define keyword->symbol (o string->symbol keyword->string))

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
      (('def (name . formals) body) `(def ,name (fn ,formals ,body)))
           
      (('do) `(centring.intr/void))
      (('do stmt) stmt)
      (('do stmt . stmts) `((fn (,(gensym '_)) (do ,@stmts)) ,stmt))
      (('fn formals body) (let ((f (gensym 'f)))
                            `(letfn ((,f ,formals ,body)) ,f)))

      ;; 
      (('if cond then else) `(centring.sf/if ,cond ,then ,else))
      (('letfn defns body)
       (let ((defns (map (lambda (defn)
                           (receive (names types) (analyze-formals (cadr defn))
                             `(,(car defn) ,names ,types ,(caddr defn))))
                         defns)))
         `(centring.sf/letfn ,defns ,body)))
      (('def name val) `(centring.sf/def ,name ,val))
      (('quote val) `(centring.sf/quote ,val))
           
      (_ sexp)))     

  (define (ctr-expand expr)
    (let ((expansion (ctr-expand-1 expr)))
      (if (eq? expansion expr)
        expr
        (ctr-expand expansion))))

  ;; Does not handle macro shadowing (i.e. (let ((if (fn (n) ...))) (if ...)))
  (define (ctr-expand-all expr)
    (let ((expansion (ctr-expand expr)))
      (if (list? expansion)
        (map ctr-expand-all (ctr-expand expr))
        expansion))))

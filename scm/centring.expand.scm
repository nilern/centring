(module centring.expand
  (expand-all)
  
  (import scheme chicken)
  (use matchable)

  ;;;;

  (define (expand-1 sexp)
    (match sexp
      (('do . stmts)
       `(centring.sf/do ,@stmts))
      (('quote val)
       `(centring.sf/quote ,val))
      
      (('ns ns-name)
       `(centring.intr/set-ns! (quote ,ns-name)))
      (('require ns-name)
       `(centring.intr/require! (quote ,ns-name)))
      (('alias ns-name as)
       `(centring.intr/alias! (quote ,ns-name) (quote ,as)))
      (('rename ns-name . binds)
       `(do
            ,@(map (lambda (bind)
                     `(centring.intr/rename! (quote ,ns-name)
                                             (quote ,(car bind))
                                             (quote ,(cadr bind))))
                   binds)))
      (('refer ns-name . names)
       `(rename ,ns-name ,@(map list names names)))
      (('import ns-name)
       `(centring.intr/import! (quote ,ns-name)))
      (('def var val)
       `(centring.intr/set-global! (quote ,var) ,val))

      (_ sexp)))

  (define (expand expr)
    (let ((expansion (expand-1 expr)))
      (if (eq? expansion expr)
        expr
        (expand expansion))))

  ;; Does not handle macro shadowing (i.e. (let ((if (fn (n) ...))) (if ...)))
  (define (expand-all expr)
    (let ((expansion (expand expr)))
      (if (list? expansion)
        (map expand-all (expand expr))
        expansion))))


(module centring.expand
  (expand-all process-pattern)
  
  (import scheme chicken)
  (use matchable
       (srfi 69)
       data-structures
       (only anaphora aif)

       centring.util)

  ;;;;

  (define (walk inner outer ast)
    (outer (if (list? ast) (map inner ast) ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

  ;;;;

  (define (expand-1 sexp)
    (match sexp
      (('do)
       '(centring.intr/rec centring.lang/Tuple))
      (('do stmt)
       stmt)
      (('do . stmts)
       `(centring.sf/do ,@stmts))
      (('quote val)
       `(centring.sf/quote ,val))
      (('letfn binds body)
       `(centring.sf/letrec
         ,(map (match-lambda
                (((name . formals) cond . body)
                 `(,name (fn (,formals ,cond ,@body)))))
               binds)
         (do ,@body)))
      (('fn . cases)
       (define (prefix-bindings binds body)
         (foldl (match-lambda*
                 ((acc (var . axpath)) `(let* ((,var ,axpath)) ,acc)))
                body binds))
       (define (replace binds e)
         (aif (assq e binds) (cdr it) e))
       (let ((arg (gensym 'x)))
         `(centring.sf/fn ,arg
            ,@(map (match-lambda
                    ((formals cond . body)
                     (receive (binds test) (process-pattern arg formals)
                       `(,(prewalk (o lower-condition (cute replace binds <>))
                                   `(and ,test ,cond))
                         ,(prefix-bindings binds `(do ,@body))))))
                   cases))))
      
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

      (('def (name . formals) cond . body)
       (let ((new-case (gensym 'f)))
         `(let* ((,new-case (fn ((centring.lang/Tuple ,@formals) ,cond ,@body))))
            (if (and (centring.intr/defined? (quote ,name))
                     (: ,name centring.lang/Fn))
              (centring.intr/fn-merge! ,name ,new-case)
              (centring.intr/set-global! (quote ,name) ,new-case)))))
      (('let* ((var val) . binds) . body)
       `(centring.intr/apply
         (centring.sf/fn ,var (#t (let* ,binds ,@body)))
         ,val))
      (('let* () . body)
       `(do ,@body))
      (('if cond then else)
       (let ((c (gensym 'c)))
         `(centring.intr/apply
           (fn (,c ,c ,then)
               (,c (not ,c) ,else))
           ,cond)))

      (_ sexp)))

  (define (process-pattern arg pat)
    (let ((patq (make-queue)))
      (define (process-1 binds tests)
        (if (queue-empty? patq)
          (values binds tests)
          (match-let (((axpath . pat) (queue-remove! patq)))
            (match pat
              ((? symbol?)
               (process-1 (cons (cons pat axpath) binds) tests))
              
              ((? literal?)
               (process-1 binds `(and ,tests (centring.lang/= ,axpath ,pat))))

              (((and (? symbol?) type) . fields)
               (let ((recname (gensym 'v)))
                 (doseq ((field-pat i) fields)
                   (queue-add! patq (cons `(centring.intr/rref ,recname ,i)
                                          field-pat)))
                 (process-1
                  (cons (cons recname axpath) binds)
                  `(and ,tests
                        (,(string->symbol "centring.lang/:") ,axpath ,type)
                        (centring.lang/= (centring.intr/rlen ,axpath)
                                         ,(length fields))))))))))

      (queue-add! patq (cons arg pat))
      (process-1 '() '(and))))

  (define lower-condition
    (match-lambda
      (('and . args) `(centring.intr/band ,@args))
      (('or . args) `(centring.intr/bior ,@args))
      (('not . args) `(centring.intr/bnot ,@args))
      (expr expr)))

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


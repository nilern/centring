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
       '(ctr.intr/rec ctr.lang/Tuple))
      (('do stmt)
       stmt)
      (('do . stmts)
       `(ctr.sf/do ,@stmts))
      (('quote val)
       `(ctr.sf/quote ,val))
      (('letfn binds body)
       `(ctr.sf/letrec
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
         `(ctr.sf/fn ,arg
            ,@(map (match-lambda
                    ((formals cond . body)
                     (receive (binds test) (process-pattern arg formals)
                       `(,(prewalk (o lower-condition (cute replace binds <>))
                                   `(and ,test ,cond))
                         ,(prefix-bindings binds `(do ,@body))))))
                   cases))))
      
      (('ns ns-name)
       `(ctr.intr/set-ns! (quote ,ns-name)))
      (('require ns-name)
       `(ctr.intr/require! (quote ,ns-name)))
      (('alias ns-name as)
       `(ctr.intr/alias! (quote ,ns-name) (quote ,as)))
      (('rename ns-name . binds)
       `(do
          ,@(map (lambda (bind)
                   `(ctr.intr/rename! (quote ,ns-name)
                                      (quote ,(car bind))
                                      (quote ,(cadr bind))))
                 binds)))
      (('refer ns-name . names)
       `(rename ,ns-name ,@(map list names names)))
      (('import ns-name)
       `(ctr.intr/import! (quote ,ns-name)))
      (('def var val)
       `(ctr.intr/set-global! (quote ,var) ,val))

      (('def (name . formals) cond . body)
       (let ((new-case (gensym 'f)))
         `(let* ((,new-case (fn ((ctr.lang/Tuple ,@formals) ,cond ,@body))))
            (if (and (ctr.intr/defined? (quote ,name))
                     (: ,name ctr.lang/Fn))
              (ctr.intr/fn-merge! ,name ,new-case)
              (ctr.intr/set-global! (quote ,name) ,new-case)))))
      (('let* ((var val) . binds) . body)
       `(ctr.intr/apply
         (ctr.sf/fn ,var (#t (let* ,binds ,@body)))
         ,val))
      (('let* () . body)
       `(do ,@body))
      (('if cond then else)
       `(ctr.intr/brf ,cond ,then ,else))
      (('and)
       #t)
      (('and atom)
       atom)
      (('and atom . ratoms)
       `(if ,atom
          (and ,@ratoms)
          #f))
      (('or)
       #f)
      (('or atom . ratoms)
       `(if ,atom
          ,atom
          (or ,@ratoms)))

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
               (process-1 binds `(and ,tests (ctr.lang/= ,axpath ,pat))))

              (((and (? symbol?) type) . fields)
               (let ((recname (gensym 'v)))
                 (doseq ((field-pat i) fields)
                   (queue-add! patq (cons `(ctr.intr/rref ,recname ,i)
                                          field-pat)))
                 (process-1
                  (cons (cons recname axpath) binds)
                  `(and ,tests
                        (,(string->symbol "ctr.lang/:") ,axpath ,type)
                        (ctr.lang/= (ctr.intr/rlen ,axpath)
                                    ,(length fields))))))))))

      (queue-add! patq (cons arg pat))
      (process-1 '() '(and))))

  (define lower-condition
    (match-lambda
      (('and . args) `(ctr.intr/band ,@args))
      (('or . args) `(ctr.intr/bior ,@args))
      (('not . args) `(ctr.intr/bnot ,@args))
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


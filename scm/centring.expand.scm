(module centring.expand
  (expand-all)
  
  (import scheme chicken)
  (use matchable
       (srfi 69)
       (only data-structures identity))

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
       ;; FIXME: prevent macroexpansion of logical connectives in conditions:
       (define (prefix-bindings axs body)
         (hash-table-fold axs (lambda (k v acc) `(let* ((,k ,v)) ,acc)) body))
       (define (replace rpls e)
         (hash-table-ref/default rpls e e))
       (let ((arg (gensym 'x)))
         `(centring.sf/fn ,arg
           ,@(map (match-lambda
                   ((formals cond . body)
                    (receive (tests axs) (destructure arg formals)
                      `(,(lower-condition
                          `(and ,@tests
                                ,(postwalk (cute replace axs <>) cond)))
                        ,(prefix-bindings axs `(do ,@body))))))
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
         `(let* ((,new-case (fn (,formals ,cond ,@body))))
            (if (and (centring.intr/defined? (quote ,name))
                     (: ,name centring.lang/Fn))
              (centring.intr/fn-merge! ,name ,new-case)
              (centring.intr/set-global! (quote ,name) ,new-case)))))
      (('let* ((var val) . binds) . body)
       `(centring.intr/apply
         (fn (,var #t (let* ,binds ,@body)))
         ,val))
      (('let* () . body)
       `(do ,@body))
      (('if cond then else)
       (let ((c (gensym 'c)))
         `(centring.intr/apply
           (fn (,c (= ,c #f) ,else) (,c (not (= ,c #f)) ,then))
           ,cond)))

      (_ sexp)))

  (define (destructure arg pat)
    ;; ATM just does a non-nested arglist without varargs i.e. (x y z)
    (let ((accesses (make-hash-table)))
      (if (symbol? pat)
        (begin
          (hash-table-set! accesses pat arg)
          (values '() accesses))
        (let recur ((pat pat) (i 0))
          (if (null? pat)
            (values `((: ,arg centring.lang/Tuple)
                      (= (centring.intr/rlen ,arg) ,i))
                    accesses)
            (begin
              (hash-table-set! accesses (car pat)
                               `(centring.intr/rref ,arg ,i))
              (recur (cdr pat) (add1 i))))))))

  (define (lower-condition cond)
    (define lower
      (match-lambda
       (('and . args) `(centring.intr/band ,@args))
       (('or . args) `(centring.intr/bior ,@args))
       (('not . args) `(centring.intr/bnot ,@args))
       ((': . args) `(centring.intr/inst? ,@args))
       (('= . args) `(centring.intr/bit-eq? ,@args))
       (expr expr)))
    (postwalk lower cond))

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


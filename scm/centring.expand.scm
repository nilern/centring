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
       (define (prefix-bindings axs body)
         (hash-table-fold axs (lambda (k v acc) `(let* ((,k ,v)) ,acc)) body))
       (define (replace rpls e)
         (hash-table-ref/default rpls e e))
       (let ((arg (gensym 'x)))
         `(centring.sf/fn ,arg
           ,@(map (match-lambda
                   ((formals cond . body)
                    (receive (ltest axs) (destructure arg formals)
                      `((and ,ltest ,(postwalk (cute replace axs <>) cond))
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
              (centring.intr/set-global! ,name ,new-case)))))
      (('let* ((var val) . binds) . body)
       `(centring.intr/apply
         (centring.sf/fn ,var (#t (let* ,binds ,@body)))
         ,val))
      (('let* () . body)
       `(do ,@body))
      (('if cond then else)
       (let ((c (gensym 'c)))
         `(centring.intr/apply
           (centring.sf/fn ,c (,c ,then) (#t ,else))
           ,cond)))

      (_ sexp)))

  (define (destructure arg pat)
    ;; ATM just does a non-nested arglist without varargs i.e. (x y z)
    (let ((accesses (make-hash-table)))
      (let recur ((pat pat) (i 0))
        (if (null? pat)
          (values `(= (centring.intr/block-length ,arg) ,i) accesses)
          (begin
            (hash-table-set! accesses (car pat)
                             `(centring.intr/block-ref ,arg ,i))
            (recur (cdr pat) (add1 i)))))))

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


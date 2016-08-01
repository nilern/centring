(library (ctr expand)
  (export expand-all)
  (import (rnrs (6)))

  (define (expand-1 expr)
    (if (pair? expr)
      (case (car expr)
        ;; Special form veneer:
        ((letfn) (expand-letfn (cdr expr)))
        ((do) (expand-do (cdr expr)))
        ((quote) (expand-quote (cdr expr)))
        
        ;; Nothing to expand:
        (else expr))
      expr))

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
        expansion)))

  ;;;;

  (define (expand-do stmts)
    ;; MAYBE: Move the optimization to someplace more appropriate?
    (case (length stmts)
      ((0) '(ctr.intr/rec ctr.lang/Tuple))
      ((1) (car stmts))
      (else (cons 'ctr.sf/do stmts))))

  (define (expand-quote args)
    `(ctr.sf/quote ,(car args)))

  (define (expand-letfn args)
    `(ctr.sf/letrec
      ,(map
        (lambda (clause)
          `(,(car clause) (fn (,(cadr clause) ,(caddr clause)
                               ,@(cdddr clause)))))
        (car args))
      (do ,@(cdr args)))))

;;   ;;;;

;;   (define (walk inner outer ast)
;;     (outer (if (list? ast) (map inner ast) ast)))

;;   (define (postwalk f ast)
;;     (walk (cute postwalk f <>) f ast))

;;   (define (prewalk f ast)
;;     (walk (cute prewalk f <>) identity (f ast)))

;;   ;;;;

;;   (define (expand-1 sexp)
;;       (('fn . cases)
;;        (define (prefix-bindings binds body)
;;          (foldl (match-lambda*
;;                  ((acc (var . axpath)) `(let* ((,var ,axpath)) ,acc)))
;;                 body binds))
;;        (define (replace binds e)
;;          (aif (assq e binds) (cdr it) e))
;;        (let ((arg (gensym 'x)))
;;          `(ctr.sf/fn ,arg
;;             ,@(map (match-lambda
;;                     ((formals cond . body)
;;                      (receive (binds test) (process-pattern arg formals)
;;                        `(,(prewalk (o lower-condition (cute replace binds <>))
;;                                    `(and ,test ,cond))
;;                          ,(prefix-bindings binds `(do ,@body))))))
;;                    cases))))
      
;;       (('ns ns-name)
;;        `(ctr.intr/set-ns! (quote ,ns-name)))
;;       (('require ns-name)
;;        `(ctr.intr/require! (quote ,ns-name)))
;;       (('alias ns-name as)
;;        `(ctr.intr/alias! (quote ,ns-name) (quote ,as)))
;;       (('rename ns-name . binds)
;;        `(do
;;           ,@(map (lambda (bind)
;;                    `(ctr.intr/rename! (quote ,ns-name)
;;                                       (quote ,(car bind))
;;                                       (quote ,(cadr bind))))
;;                  binds)))
;;       (('refer ns-name . names)
;;        `(rename ,ns-name ,@(map list names names)))
;;       (('import ns-name)
;;        `(ctr.intr/import! (quote ,ns-name)))
      
;;       (('def var val)
;;        `(ctr.intr/set-global! (quote ,var) ,val))
;;       (('defn name . cases)
;;        (let ((new-cases (gensym 'f)))
;;          `(let* ((,new-cases (fn ,@cases)))
;;             (if (and (ctr.intr/defined? (quote ,name))
;;                      (,(string->symbol "ctr.lang/:") ,name ctr.lang/Fn))
;;               (ctr.intr/fn-merge! ,name ,new-cases)
;;               (ctr.intr/set-global! (quote ,name) ,new-cases)))))

;;       (('ffi-require libname)
;;        `(ctr.intr/ffi-require ,libname))
;;       (('ffi-fn ret-type sym)
;;        `(ctr.intr/ffi-fn ,ret-type (quote ,sym)))
      
;;       (('match matchee . cases)
;;        `(ctr.intr/apply (fn ,@cases) ,matchee))
;;       (('let ((pat val) . binds) . body)
;;        `(ctr.intr/apply (fn (,pat #t (let ,binds ,@body)))
;;                         ,val))
;;       (('let () . body)
;;        `(do ,@body))
;;       (('let* ((var val) . binds) . body)
;;        `(ctr.intr/apply
;;          (ctr.sf/fn ,var (#t (let* ,binds ,@body)))
;;          ,val))
;;       (('let* () . body)
;;        `(do ,@body))
;;       (('let-cc k . body)
;;        `(ctr.intr/apply-cc (fn (,k #t ,@body))))
;;       (('if cond then else)
;;        `(ctr.intr/brf ,cond ,then ,else))
;;       (('and)
;;        #t)
;;       (('and atom)
;;        atom)
;;       (('and atom . ratoms)
;;        `(if ,atom
;;           (and ,@ratoms)
;;           #f))
;;       (('or)
;;        #f)
;;       (('or atom . ratoms)
;;        `(if ,atom
;;           ,atom
;;           (or ,@ratoms)))

;;       ;; FIXME: DRY:
;;       (('defrecord (name fields ... ((? (cute eq? '... <>)) rest-field)))
;;        (let ((T (gensym 'T))
;;              (v (gensym 'v))
;;              (i (gensym 'i))
;;              (i* (gensym 'i*))
;;              (args (gensym 'args)))
;;          `(do
;;             ;; TODO: ns-qualify name here:
;;             (def ,name (ctr.lang/new ctr.lang/Type (quote ,name)))
;;             ;; TODO: use pattern matching here when it has more features: 
;;             (ctr.intr/fn-merge!
;;              ctr.lang/new
;;              (fn
;;               (,args
;;                (and (: ,args ctr.lang/Tuple)
;;                     (ctr.intr/igt? (ctr.intr/rlen ,args) ,(length fields))
;;                     (ctr.intr/identical? (ctr.intr/rref ,args 0) ,name))
;;                (ctr.intr/shrec ,args))))
;;             ,@(smap*
;;                '()
;;                (lambda (coll iter)
;;                  (let ((i (index iter))
;;                        (field (elt coll iter)))
;;                    `(defn ,(symbol-append '|.| field)
;;                       ((ctr.lang/Tuple ,v) (: ,v ,name)
;;                        (ctr.intr/rref ,v ,i)))))
;;                fields)
;;             ;; TODO: check type of i, add error message:
;;             (defn ,(symbol-append '|.| rest-field)
;;               ((ctr.lang/Tuple ,v ,i) (: ,v ,name)
;;                (let* ((,i* (ctr.intr/iadd ,(length fields) ,i)))
;;                  (if (and (ctr.intr/ige? ,i* ,(length fields))
;;                           (ctr.intr/ilt? ,i* (ctr.intr/rlen ,v)))
;;                    (ctr.intr/rref ,v (ctr.intr/iadd ,(length fields) ,i))
;;                    (ctr.intr/err 'BoundsError #f))))))))
;;       (('defrecord (name . fields))
;;        (let ((T (gensym 'T))
;;              (v (gensym 'v))
;;              (args (gensym 'args)))
;;          `(do
;;             ;; TODO: ns-qualify name here:
;;             (def ,name (ctr.lang/new ctr.lang/Type (quote ,name)))
;;             ;; TODO: use pattern matching here when it has more features:  
;;             (ctr.intr/fn-merge!
;;              ctr.lang/new
;;              (fn
;;               (,args
;;                (and (: ,args ctr.lang/Tuple)
;;                     (ctr.intr/ieq? (ctr.intr/rlen ,args) ,(add1 (length fields)))
;;                     (ctr.intr/identical? (ctr.intr/rref ,args 0) ,name))
;;                (ctr.intr/shrec ,args))))
;;             ,@(smap*
;;                '()
;;                (lambda (coll iter)
;;                  (let ((i (index iter))
;;                        (field (elt coll iter)))
;;                    `(defn ,(symbol-append '|.| field)
;;                       ((ctr.lang/Tuple ,v) (: ,v ,name)
;;                       (ctr.intr/rref ,v ,i)))))
;;                fields))))
                 
;;       (_ sexp)))

;;   (define (process-pattern arg pat)
;;     (let ((patq (make-queue)))
;;       (define (process-1 binds tests)
;;         (if (queue-empty? patq)
;;           (values binds tests)
;;           (match-let (((axpath . pat) (queue-remove! patq)))
;;             (match pat
;;               ((? symbol?)
;;                (process-1 (cons (cons pat axpath) binds) tests))
              
;;               ((? literal?)
;;                (process-1 binds `(and ,tests (ctr.lang/= ,axpath ,pat))))

;;               (('and . pats)
;;                (doseq (pat pats)
;;                  (queue-add! patq (cons axpath pat)))
;;                (process-1 binds tests))

;;               (((and (? symbol?) type) . fields)
;;                (let ((recname (gensym 'v)))
;;                  (doseq ((field-pat i) fields)
;;                    (queue-add! patq (cons `(ctr.intr/rref ,recname ,i)
;;                                           field-pat)))
;;                  (process-1
;;                   (cons (cons recname axpath) binds)
;;                   `(and ,tests
;;                         (: ,axpath ,type)
;;                         (ctr.intr/ieq? (ctr.intr/rlen ,axpath)
;;                                        ,(length fields))))))
;;               (_ (error "unrecognized pattern" pat))))))

;;       (queue-add! patq (cons arg pat))
;;       (process-1 '() '(and))))

;;   (define lower-condition
;;     (match-lambda
;;       (('and . args) `(ctr.intr/band ,@args))
;;       (('or . args) `(ctr.intr/bior ,@args))
;;       (('not . args) `(ctr.intr/bnot ,@args))
;;       ((': v T) `(ctr.intr/identical? (ctr.intr/type ,v) ,T))
;;       (expr expr)))


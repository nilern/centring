(library (ctr expand)
  (export expand-all)
  (import (rnrs (6))
          (only (chezscheme) gensym)

          (only (util) if-let dolist partial comp identity)
          (only (util collections) reduce into)
          (only (util queue) make-queue queue-empty? enqueue! queue-pop!)

          (only (ctr util) ctr-error literal?))

  ;;; TODO: throw lots of syntax errors

  (define (expand-1 expr)
    (if (pair? expr)
      (case (car expr)
        ;; Special form veneer:
        ((fn)    (expand-fn (cdr expr)))
        ((letfn) (expand-letfn (cdr expr)))
        ((do)    (expand-do (cdr expr)))
        ((quote) (expand-quote (cdr expr)))

        ;; Intrinsic veneer:
        ((ns)   (apply expand-ns (cdr expr)))
        ((def)  (expand-def (cdr expr)))
        ((defn) (expand-defn (cdr expr)))
        ((if)   (apply expand-if (cdr expr)))

        ;; Binding forms:
        ((let*)  (apply expand-let* (cdr expr)))
        ((let)   (apply expand-let (cdr expr)))
        ((match) (apply expand-match (cdr expr)))

        ;; Require:
        ((require) (expand-require (cdr expr)))

        ;; Conveniences:
        ((->) (expand--> (cdr expr)))
        
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

  ;;;; Special Form Veneer

  (define (expand-fn cases)
    (define (prefix-bindings binds body)
      (reduce
       (lambda (acc binding)
         `(let* ((,(car binding) ,(cdr binding))) ,acc))
       body binds))
    (define (replace binds expr)
      (if-let (binding (assq expr binds))
        (cdr binding)
        expr))
    (let ((arg (gensym "x")))
      `(ctr.sf/fn ,arg
         ,@(map
            (lambda (case)
              (let ((formals (car case))
                    (cond (cadr case))
                    (body (cddr case)))
                (let-values (((binds test) (process-pattern arg formals)))
                  `(,(prewalk (comp lower-condition (partial replace binds))
                              `(and ,test ,cond))
                    ,(prefix-bindings binds `(do ,@body))))))
            cases))))

  (define (expand-letfn args)
    `(ctr.sf/letrec
      ,(map
        (lambda (clause)
          `(,(car clause) (fn (,(cadr clause) ,(caddr clause)
                               ,@(cdddr clause)))))
        (car args))
      (do ,@(cdr args))))

  (define (expand-do stmts)
    ;; MAYBE: Move the optimization to someplace more appropriate?
    (case (length stmts)
      ((0) '(ctr.intr/rec ctr.lang/Tuple))
      ((1) (car stmts))
      (else (cons 'ctr.sf/do stmts))))

  (define (expand-quote args)
    `(ctr.sf/quote ,(car args)))

  ;;;; Intrinsic Veneer

  (define (expand-ns ns)
    `(ctr.intr/set-ns! (quote ,ns)))

  (define (expand-def args)
    `(ctr.intr/set-global! (quote ,(car args)) ,(cadr args)))

  (define (expand-defn args)
    (let ((name (car args))
          (cases (cdr args))
          (new-cases (gensym "f")))
      `(let* ((,new-cases (fn ,@cases)))
         (if (and (ctr.intr/defined? (quote ,name))
                  (,(string->symbol "ctr.lang/:") ,name ctr.lang/Fn))
           (ctr.intr/fn-merge! ,name ,new-cases)
           (ctr.intr/set-global! (quote ,name) ,new-cases)))))

  (define (expand-if cond then else)
    `(ctr.intr/brf ,cond ,then ,else))

  ;;;; Binding Forms

  (define (expand-let* binds . body)
    (if (null? binds)
      `(do ,@body)
      `(ctr.intr/apply
        (ctr.sf/fn ,(caar binds) (#t (let* ,(cdr binds) ,@body)))
        ,(cadar binds))))

  (define (expand-let binds . body)
    (if (null? binds)
      `(do ,@body)
      `(ctr.intr/apply (fn (,(caar binds) #t (let ,(cdr binds) ,@body)))
                       ,(cadar binds))))

  (define (expand-match matchee . cases)
    `(ctr.intr/apply (fn ,@cases) ,matchee))

  ;;;; Require

  (define (expand-require clauses)
    (define (handle-clause clause)
      (if (symbol? clause)
        (values `((ctr.intr/require! (quote ,clause))) clause)
        (let-values (((actions req-ns) (handle-clause (cadr clause))))
          (if (eq? (car clause) '->)
            (handle-clause (expand--> (cdr clause)))
            (values
             (case (car clause)
               ((as)
                (cons `(ctr.intr/alias! (quote ,req-ns) (quote ,(caddr clause)))
                      actions))
               ((use)
                (cons `(ctr.intr/start-import! (quote ,req-ns)) actions))
               ((only)
                (cons `(ctr.intr/refer! #t ,@(map (lambda (name) `(quote ,name))
                                                  (cddr clause)))
                      actions))
               ((except)
                (cons `(ctr.intr/refer! #f ,@(map (lambda (name) `(quote ,name))
                                                  (cddr clause)))
                      actions))
               ((rename)
                ;; TODO: use transducer:
                (into actions
                      (map (lambda (nr)
                             `(ctr.intr/rename! (quote ,(car nr))
                                                (quote ,(cadr nr))))
                           (cddr clause))))
               (else (ctr-error "unsupported require operator" (car clause))))
             req-ns)))))
      ;; TODO: use transduce:
    `(do
       ,@(reduce into '()
                 (map (lambda (clause)
                        (let-values (((actions _) (handle-clause clause)))
                          (cons '(ctr.intr/end-import!) actions)))
                      (reverse clauses)))))

  ;;;; Conveniences

  (define (expand--> args)
    (let recur ((res (car args)) (pipeline (cdr args)))
      (cond
       ((null? pipeline)
        res)
       ((symbol? (car pipeline))
        (recur `(,(car pipeline) ,res) (cdr pipeline)))
       ((pair? (car pipeline))
        (recur `(,(caar pipeline) ,res ,@(cdar pipeline)) (cdr pipeline)))
       (else
        (ctr-error "cannot `->` through" (car pipeline))))))

  ;;;;

  (define (walk inner outer ast)
    (outer (if (list? ast) (map inner ast) ast)))

  (define (postwalk f ast)
    (walk (partial postwalk f) f ast))

  (define (prewalk f ast)
    (walk (partial prewalk f) identity (f ast)))

  (define (process-pattern arg pat)
    (let ((patq (make-queue)))
      (letrec ((process-1
                (lambda (binds tests)
                  (if (queue-empty? patq)
                    (values binds tests)
                    (let* ((ax-pat (queue-pop! patq #f))
                           (axpath (car ax-pat))
                           (pat (cdr ax-pat)))
                      (cond
                       ((symbol? pat)
                        (process-1 (cons (cons pat axpath) binds) tests))

                       ((literal? pat)
                        (process-1 binds `(and ,tests (ctr.lang/= ,axpath ,pat))))

                       ((eq? (car pat) 'and)
                        (dolist (pat (cdr pat))
                          (enqueue! patq (cons axpath pat)))
                        (process-1 binds tests))

                       ((or (eq? (car pat) 'ctr.lang/new)
                            (eq? (car pat) 'new))
                        (let ((recname (gensym "v"))
                              (type (cadr pat))
                              (fields (cddr pat)))
                          (dolist ((field-pat i) fields)
                            (enqueue! patq (cons `(ctr.intr/rref ,recname ,i)
                                                 field-pat)))
                          (process-1
                           (cons (cons recname axpath) binds)
                           `(and ,tests
                                 (: ,axpath ,type)
                                 (ctr.intr/ieq? (ctr.intr/rlen ,axpath)
                                                ,(length fields))))))

                       (else (ctr-error "unrecognized pattern" pat))))))))
        (enqueue! patq (cons arg pat))
        (process-1 '() '(and)))))

  (define (lower-condition expr)
    (if (list? expr)
      (let ((op (car expr))
            (args (cdr expr)))
        (case op
          ((and) `(ctr.intr/band ,@args))
          ((or)  `(ctr.intr/bior ,@args))
          ((not) `(ctr.intr/bnot ,@args))
          ((:) (let ((v (car args))
                     (T (cadr args)))
                 `(ctr.intr/identical? (ctr.intr/type ,v) ,T)))
          (else expr)))
      expr)))

;;   ;;;;

;;   (define (expand-1 sexp)

;;       (('ffi-require libname)
;;        `(ctr.intr/ffi-require ,libname))
;;       (('ffi-fn ret-type sym)
;;        `(ctr.intr/ffi-fn ,ret-type (quote ,sym)))

;;       (('let-cc k . body)
;;        `(ctr.intr/apply-cc (fn (,k #t ,@body))))
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


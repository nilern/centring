(module centring.expand
  (expand-all process-pattern)
  
  (import scheme chicken)
  (use matchable
       sequences
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
       ;; FIXME: get back to (current-ns) when done evaluating require:d ns
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

      (('deftype (name . fields))
       ;; TODO: rest-fields
       (let ((T (gensym 'T))
             (v (gensym 'v))
             (args (gensym 'args)))
         `(do
            ;; TODO: ns-qualify name here:
            (def ,name (ctr.lang/new ctr.lang/Type (quote ,name)))
            ;; TODO: use pattern matching here when it has more features:  
            (ctr.intr/fn-merge!
             ctr.lang/new
             (fn (,args
               (and (: ,args ctr.lang/Tuple)
                    (ctr.intr/ieq? (ctr.intr/rlen ,args) ,(length fields))
                    (ctr.intr/identical? (ctr.intr/rref ,args 0) ,name))
               (ctr.intr/shrec ,args))))
            ,@(smap*
               '()
               (lambda (coll iter)
                 (let ((i (index iter))
                       (field (elt coll iter)))
                   `(def (,(symbol-append '|.| field) ,v) (: ,v ,name)
                      (ctr.intr/rref ,v ,i))))
               fields)
            )))
               
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
                        (: ,axpath ,type)
                        (ctr.intr/ieq? (ctr.intr/rlen ,axpath)
                                       ,(length fields))))))
              (_ (error "unrecognize pattern" pat))))))

      (queue-add! patq (cons arg pat))
      (process-1 '() '(and))))

  (define lower-condition
    (match-lambda
      (('and . args) `(ctr.intr/band ,@args))
      (('or . args) `(ctr.intr/bior ,@args))
      (('not . args) `(ctr.intr/bnot ,@args))
      ((': v T) `(ctr.intr/identical? (ctr.intr/type ,v) ,T))
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


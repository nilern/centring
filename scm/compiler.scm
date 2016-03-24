(module centring.compiler
  *

  (import scheme chicken)
  (use (only matchable match match-let)
       (only (srfi 69)
             make-hash-table
             hash-table-set!
             hash-table-ref/default)
       (only data-structures identity constantly))

  ;;;; CPS AST

  (define-record Var name)
  
  (define-record Const val)

  (define-record If
    cond
    tcont
    fcont)

  (define-record Fix
    bindings
    body)

  (define-record App
    callee
    args)

  ;;;; CPS Conversion

  (define (cps-k sexpr c)
    (match sexpr
      (`(centring.sf/if ,cond ,then ,else)
       (cps-k cond (lambda (cast)
                     (let* ((k (gensym 'k))
                            (v (gensym 'v))
                            (app-k (lambda (v) (make-App (make-Var k) `(,v)))))
                       (make-Fix
                        `((,k (,v) (centring.lang/Any) ,(c (make-Var v))))
                        (make-If cast
                                 (cps-k then app-k)
                                 (cps-k else app-k)))))))

      (`(centring.sf/letfn ,bindings ,body)
       (make-Fix (cps-bindings bindings) (cps-k body c)))
      
      (`(,callee . ,args)
       (cps-k callee (lambda (f)
                       (let* ((r (gensym 'r))
                              (v (gensym 'v)))
                         (make-Fix
                          `((,r (,v) (centring.lang/Any)
                                ,(c (make-Var v))))
                          (cps-list args
                                    (lambda (as)
                                      (make-App f (cons (make-Var r) as)))))))))
                      
      ((and id (? symbol?)) (c (make-Var id)))
      ((and v (? fixnum?)) (c (make-Const v)))))

  (define (cps-list sexprs c)
    (letrec ((cpsl (lambda (sexprs res)
                     (match sexprs
                       (`(,sexpr . ,sexprs)
                        (cps-k sexpr
                               (lambda (v)
                                 (cpsl sexprs (cons v res)))))
                       ('() (c (reverse res)))))))
      (cpsl sexprs '())))

  (define (cps-bindings bindings)
    (match bindings
      (`((,name ,formals ,types ,body) . ,bindings)
       (let ((r (gensym 'r)))
         (cons `(,name ,(cons r formals) ,(cons 'centring.lang/Any types)
                       ,(cps-k body (lambda (v)
                                (make-App (make-Var r) (list v)))))
               (cps-bindings bindings))))
      ('() '())))

  ;;;; CPS AST Traversal

  (define (fmap f ast)
    (match ast
      (($ If cond tcont fcont) (make-If (f cond) (f tcont) (f fcont)))
      (($ Fix defns body)
       (make-Fix (map (lambda (defn)
                        (match-let ((`(,name ,formals ,types ,body) defn))
                          `(,name ,formals ,types ,(f body))))
                      defns)
                 (f body)))
      (($ App callee args) (make-App (f callee) (map f args)))
      (_ ast)))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

  ;;;; Debugging utilities

  (define (cps->sexp ast)
    (match ast
      (($ If cond tcont fcont) `(centring.sf/if ,(cps->sexp cond)
                                                ,(cps->sexp tcont)
                                                ,(cps->sexp fcont)))
      (($ Fix bindings body)
       `(centring.sf/letfn
         ,(map (lambda (b)
                 (match-let ((`(,name ,args ,types ,bbody) b))
                   `(,name ,args ,types ,(cps->sexp bbody))))
               bindings)
         ,(cps->sexp body)))
      (($ App callee args) `(,(cps->sexp callee) ,@(map cps->sexp args)))
      (($ Var name) name)
      (($ Const val) val)))

  ;;;; Utility Passes

  (define (replace-var-uses replacements ast)
    (match ast
      (($ Var name) (make-Var (hash-table-ref/default replacements name name)))
      (_ ast)))

  ;;;; Eta-Contraction

  (define (eta-defn-replacements replacements ast)
    (match ast
      (($ Fix defns body)
       (letrec ((formals=args?
                 (lambda (formals args)
                   (match `(,formals ,args)
                     ('(() ()) #t)
                     (`((,formal . ,rformals) (,($ Var arg) . ,rargs))
                      (and (eq? formal arg) (formals=args? rformals rargs)))
                     (_ #f))))
                (handle-defns
                 (lambda (defns)
                   (match defns
                     ;; TODO: confirm that the formal-types of name and callee
                     ;; are the same and that name isn't one of the formals:
                     (`(,(and defn
                              `(,name ,formals ,_ ,($ App ($ Var callee) args)))
                        . ,rdefns)
                      (when (formals=args? formals args)
                        (hash-table-set! replacements name callee))
                      (cons defn (handle-defns rdefns)))
                     (`(,defn . ,rdefns) (cons defn (handle-defns rdefns)))
                     ('() '())))))
         (make-Fix (handle-defns defns) body)))
      (_ ast)))

  ;; TODO: Make this cascade all the way down in a reasonable number of passes
  ;; (one?), (maybe the generic walk can't do that?):
  (define (eta-contract ast)
    (let ((replacements (make-hash-table)))
      (prewalk (lambda (node)
                  (replace-var-uses replacements
                                    (eta-defn-replacements replacements node)))
               ast)))

  ;;;; Elimination of Unused Variables

  ;; Isn't this actually some sort of fold?
  (define (count-uses varnames ast)
    (let ((zeros (map (constantly 0) varnames)))
      (match ast
        (($ If cond tcont fcont) (map +
                                      (count-uses varnames cond)
                                      (count-uses varnames tcont)
                                      (count-uses varnames fcont)))
        (($ Fix defns body) (map +
                                 (foldl
                                  (lambda (acc defn)
                                    (map + acc (count-uses varnames (cadddr defn))))
                                  zeros defns)
                                 (count-uses varnames body)))
        (($ App callee args) (map +
                                  (count-uses varnames callee)
                                  (foldl (lambda (acc arg)
                                           (map + acc (count-uses varnames arg)))
                                         zeros args)))
        (($ Var name) (map (lambda (varname) (if (eq? name varname) 1 0))
                           varnames))
        (($ Const _) zeros))))

  ;; OPTIMIZE: this is probably slow since count-uses is used so much:
  (define (remove-unuseds ast)
    (postwalk
     (lambda (node)
       (match node
         (($ Fix defns body)
          (letrec ((remove-udefns
                    (lambda (defns uses)
                      (cond
                       ((null? defns) '())
                       ((zero? (car uses)) (remove-udefns (cdr defns) (cdr uses)))
                       (else (cons (car defns)
                                   (remove-udefns (cdr defns) (cdr uses))))))))
            (let* ((uses (count-uses (map car defns) body))
                   (defns* (remove-udefns defns uses)))
              (if (null? defns*)
                body
                (make-Fix defns* body)))))
         (_ node)))
     ast)))

(module centring.compiler
  *

  (import scheme chicken)
  (use (only matchable match match-let))

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
      
      (`(,callee . ,args)
       (cps-k callee (lambda (f)
                       (let* ((r (gensym 'r))
                              (v (gensym 'v)))
                         (make-Fix
                          `((,r (,v) (centring.lang/Any) ,(c (make-Var v))))
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
      (($ Const val) val)
      (_ (error "Invalid CPS-expression" ast)))))

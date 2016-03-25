(module centring.compiler
  *

  (import scheme chicken)
  (use (only matchable match match-let)
       (only (srfi 69)
             make-hash-table
             alist->hash-table
             hash-table-set!
             hash-table-ref
             hash-table-ref/default
             hash-table-exists?)
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

  (define-record Def
    name
    val
    cont)

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

      (`(centring.sf/fn ,formals ,types ,body)
       (let ((f (gensym 'f)))
         (cps-k `(centring.sf/letfn ((,f ,formals ,types ,body)) ,f) c)))

      (`(centring.sf/letfn ,bindings ,body)
       (make-Fix (cps-bindings bindings) (cps-k body c)))

      (`(centring.sf/def ,name ,expr)
       (cps-k expr (lambda (e)
                     (make-Def name e
                               (cps-k '(centring.lang/Tuple) c)))))
      
      (`(centring.sf/do . ,stmts)
       (letrec ((monadize (lambda (stmts)
                            (match stmts
                              ('() '(centring.lang/Tuple))
                              ((stmt) stmt)
                              ((stmt . rstmts)
                               (let ((_ (gensym '_)))
                                 `((centring.sf/fn (,_) (centring.lang/Any)
                                     ,(monadize rstmts))
                                   ,stmt)))))))
         (cps-k (monadize stmts) c)))
      
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
      (($ Def name val cont) (make-Def name (f val) (f cont)))
      (($ App callee args) (make-App (f callee) (map f args)))
      ((or (? Var?) (? Const?)) ast)))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

  (define (fold-leaves f acc ast)
    (match ast
      (($ If cond tcont fcont)
       (let* ((acc* (fold-leaves f acc cond))
              (acc** (fold-leaves f acc* tcont)))
         (fold-leaves f acc** fcont)))
      
      (($ Fix defns body)
       (let ((acc* (foldl (lambda (acc defn)
                            (fold-leaves f acc (cadddr defn)))
                          acc defns)))
         (fold-leaves f acc* body)))

      (($ Def name val cont)
       (let ((acc* (fold-leaves f acc val)))
         (fold-leaves f acc* cont)))
      
      (($ App callee args)
       (let ((acc* (fold-leaves f acc callee)))
         (foldl (lambda (acc arg) (fold-leaves f acc arg)) acc* args)))
       
      ((or (? Var?) (? Const?)) (f acc ast))))

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
      (($ Def name val cont)
       `(centring.sf/def ,name ,(cps->sexp val) ,(cps->sexp cont)))
      (($ App callee args) `(,(cps->sexp callee) ,@(map cps->sexp args)))
      (($ Var name) name)
      (($ Const val) val)))

  ;;;; Utility Passes

  (define (count-uses varnames ast)
    (letrec ((c-u (lambda (acc node)
                    (match node
                      (($ Var name) (map (lambda (count varname)
                                           (if (eq? varname name)
                                             (+ count 1)
                                             count))
                                         acc
                                         varnames))
                      ((? Const?) acc)))))
      (fold-leaves c-u (map (constantly 0) varnames) ast)))

  (define (replace-var-uses replacements ast)
    (match ast
      (($ Var name) (hash-table-ref/default replacements name ast))
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
                (handle-defn
                 (lambda (defn)
                   (match-let ((`(,name ,formals ,types ,body) defn))
                      ;; Replacing the body here lets us contract chains of
                      ;; eta-redexes in one pass of `eta-contract`:
                      (let ((body* (postwalk
                                    (cute replace-var-uses replacements <>)
                                    body)))
                        (match body*
                          ;; TODO: confirm that the formal-types of name and callee
                          ;; are the same and that name isn't one of the formals:
                          (($ App callee args)
                           (when (formals=args? formals args)
                             (hash-table-set! replacements name callee)))
                          (_))
                        `(,name ,formals ,types ,body*))))))
         (make-Fix (map handle-defn defns) body)))
      (_ ast)))

  (define (eta-contract ast)
    (let ((replacements (make-hash-table)))
      (prewalk (lambda (node)
                  (replace-var-uses replacements
                                    (eta-defn-replacements replacements node)))
               ast)))

  ;;;; Beta-Contraction

  (define (add-inlinables! inlinables node)
    (match node
      (($ Fix defns body)
       ;; FIXME: If the only use is recursive, this will result in infinite
       ;; expansion! Just leave those alone (they will be removed anyway).
       (let ((uses (count-uses (map car defns) node)))
         (map (lambda (usecount defn)
                (when (= usecount 1)
                  (hash-table-set! inlinables (car defn) (cdr defn))))
              uses defns)))
      (_)))

  (define (inline inlinables node)
    (match node
      (($ App ($ Var callee) args)
       ;; TODO: check that there can be no type errors from this call:
       (if (hash-table-exists? inlinables callee)
         (match-let (((formals _ body) (hash-table-ref inlinables callee)))
           (if (= (length formals) (length args))
             (let* ((f-as (alist->hash-table
                           (map (lambda (formal arg) (cons formal arg))
                                formals args)))
                    (body* (postwalk (cute replace-var-uses f-as <>) body)))
               body*)
             node))
         node))
      (_ node)))
       
  (define (beta-contract ast)
    (let ((inlinables (make-hash-table)))
      (prewalk (lambda (node)
                 (add-inlinables! inlinables node)
                 (inline inlinables node))
               ast)))

  ;;;; Elimination of Unused Variables

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

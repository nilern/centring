(module centring.cps
  *

  (import scheme chicken)
  (use (only matchable match match-let match-let* match-lambda)
       (only anaphora aif)
       (only (srfi 1) remove list-index map-in-order)
       (only (srfi 69)
             make-hash-table
             alist->hash-table
             hash-table->alist
             hash-table-set!
             hash-table-ref
             hash-table-ref/default
             hash-table-exists?)
       (only (srfi 13) string-prefix? string-index)
       (only vector-lib vector-index vector-for-each)
       (only data-structures identity constantly)
       (only miscmacros push!)
       (only format format))

  (import (prefix centring.coreast cast:))

  ;;;; Utils

  (define (every? pred? ls)
    (match ls
      ('() #t)
      ((v . vs) (and (pred? v) (every? pred? vs)))))

  (define (every-2? pred? ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else (and (pred? (car ls1) (car ls2))
                (every-2? pred? (cdr ls1) (cdr ls2))))))

  (define (lset-push ls v)
    (if (member v ls) ls (append ls (list v))))
  (define (lset-append ls1 ls2)
    (append ls1 (remove (lambda (v) (member v ls1)) ls2)))

  ;;;; CPS AST

  (define-record If cond tcont fcont)
  (define-record Fix bindings body)
  (define-record Close bindings body)
  (define-record Def name val cont)
  (define-record Primop op args results conts)
  (define-record App callee args)
  
  (define-record Local name)
  (define-record Global name)
  (define-record Clover index)
  (define-record Const val)

  ;;;; CPS Conversion

  (define (cps-k cast c)
    (match cast
      (($ cast:If cond then else)
       (cps-k cond (lambda (cast)
                     (let* ((k (gensym 'k))
                            (v (gensym 'v))
                            (app-k (lambda (v) (make-App (make-Local k) `(,v)))))
                       (make-Fix
                        `((,k (,v) (centring.lang/Any) ,(c (make-Local v))))
                        (make-If cast
                                 (cps-k then app-k)
                                 (cps-k else app-k)))))))
      (($ cast:Fix defns body)
       (make-Fix (cps-bindings defns) (cps-k body c)))
      (($ cast:Def name expr)
       (cps-k expr (lambda (e)
                     (make-Def name e
                               (cps-k (cast:make-Primop 'void '()) c)))))
      (($ cast:Primop op args)
       (cps-list args (lambda (as)
                      (let ((v (gensym 'v)))
                        (make-Primop op as
                                     `(,v) `(,(c (make-Local v))))))))
      (($ cast:App callee args)
       (cps-k callee (lambda (f)
                       (let* ((r (gensym 'r))
                              (v (gensym 'v)))
                         (make-Fix
                          `((,r (,v) (centring.lang/Any)
                                ,(c (make-Local v))))
                          (cps-list args
                                    (lambda (as)
                                      (make-App f (cons (make-Local r) as)))))))))

      (($ cast:Local name) (c (make-Local name)))
      (($ cast:Global name) (c (make-Global name)))
      (($ cast:Const val) (c (make-Const val)))
      (_ (error "cannot CPS-convert" cast))))

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
                                      (make-App (make-Local r) (list v)))))
               (cps-bindings bindings))))
      ('() '())))

  ;;;; CPS AST Traversal

  (define (fmap f node)
    (match node
      (($ If cond tcont fcont) (make-If (f cond) (f tcont) (f fcont)))
      (($ Fix defns body)
       (make-Fix (map (lambda (defn)
                        (match-let ((`(,name ,formals ,types ,dbody) defn))
                          `(,name ,formals ,types ,(f dbody))))
                      defns)
                 (f body)))
      (($ Close bindings body)
       (make-Close (map (lambda (bind) (match-let ((`(,name ,label . ,cvals) bind))
                                    `(,name ,label ,@(map f cvals))))
                        bindings)
                   (f body)))
      (($ Def name val cont) (make-Def name (f val) (f cont)))
      (($ App callee args) (make-App (f callee) (map f args)))
      (($ Primop op args results conts) (make-Primop op (map f args)
                                                     results (map f conts)))
      
      ((or (? Local?) (? Global?) (? Clover?) (? Const?)) node)))

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
       (let ((acc* (foldl (lambda (acc defn) (fold-leaves f acc (cadddr defn)))
                          acc defns)))
         (fold-leaves f acc* body)))

      (($ Close bindings body)
       (let ((acc* (foldl (lambda (acc bind)
                            (foldl (cute fold-leaves f <> <>) acc (cddr bind)))
                          acc bindings)))
         (fold-leaves f acc* body)))
       
      (($ Def name val cont)
       (let ((acc* (fold-leaves f acc val)))
         (fold-leaves f acc* cont)))
      
      (($ App callee args)
       (let ((acc* (fold-leaves f acc callee)))
         (foldl (cute fold-leaves f <> <>) acc* args)))
      
      (($ Primop op args results conts)
       (let ((acc* (foldl (cute fold-leaves f <> <>) acc args)))
         (foldl (cute fold-leaves f <> <>) acc* conts)))
       
      ((or (? Local?) (? Global?) (? Clover?) (? Const?)) (f acc ast))))

  ;;;; Debugging utilities

  (define (cps->sexp ast)
    (match ast
      (($ If cond tcont fcont) `($if ,(cps->sexp cond)
                                     ,(cps->sexp tcont)
                                     ,(cps->sexp fcont)))
      (($ Fix bindings body)
       `($letfn
         ,(map (lambda (b)
                 (match-let ((`(,name ,args ,types ,bbody) b))
                   `(,name ,args ,types ,(cps->sexp bbody))))
               bindings)
         ,(cps->sexp body)))
      (($ Close bindings body)
       `($close
         ,(map (lambda (bind)
                 (match-let (((name label . cvals) bind))
                   `(,name ,label ,@(map cps->sexp cvals))))
               bindings)
         ,(cps->sexp body)))
      (($ Def name val cont)
       `($def ,name ,(cps->sexp val) ,(cps->sexp cont)))
      (($ App callee args) `(,(cps->sexp callee) ,@(map cps->sexp args)))
      (($ Primop 'halt (v) '() '()) `(%halt ,(cps->sexp v)))
      (($ Primop op args (result) (cont))
       `($let ((,result (,(string->symbol (string-append "%" (symbol->string op)))
                         ,@(map cps->sexp args))))
          ,(cps->sexp cont)))
      
      (($ Local name) `(%l ,name))
      (($ Clover index) `(%f ,index))
      (($ Global name) `(%g ,name))
      (($ Const val) `(%c ,val))
      (_ (error "unable to display as S-expr" ast))))

  ;;;; Utility Passes

  (define (count-local-uses varnames ast)
    (letrec ((c-u (lambda (acc node)
                    (match node
                      (($ Local name) (map (lambda (count varname)
                                             (if (eq? varname name)
                                               (+ count 1)
                                               count))
                                           acc
                                           varnames))
                      ((or (? Global?) (? Clover?) (? Const?)) acc)))))
      (fold-leaves c-u (map (constantly 0) varnames) ast)))

  (define (replace-local-uses replacements node)
    (match node
      (($ Local name) (hash-table-ref/default replacements name node))
      (_ node)))

  ;;;; Eta-Contraction

  (define (eta-defn-replacements replacements node)
    (match node
      (($ Fix defns body)
       (let* ((formals=args?
               (cute every-2? (lambda (formal arg)
                                (match arg
                                  (($ Local argname) (eq? formal argname))
                                  (_ #f))) <> <>))
              (handle-defn
               (lambda (defn)
                 (match-let ((`(,name ,formals ,types ,body) defn))
                   ;; Replacing the body here lets us contract chains of
                   ;; eta-redexes in one pass of `eta-contract`:
                   (let ((body* (postwalk
                                 (cute replace-local-uses replacements <>)
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
      (_ node)))

  (define (eta-contract ast)
    (let ((replacements (make-hash-table)))
      (prewalk (lambda (node)
                  (replace-local-uses replacements
                                    (eta-defn-replacements replacements node)))
               ast)))

  ;;;; Beta-Contraction

  ;; TODO: Figure out other cases where inlining does not bypass an argument
  ;; type error and is thus permitted

  (define (add-inlinables! inlinables node)
    (match node
      (($ Fix defns body)
       ;; FIXME: If the only use is recursive, this will result in infinite
       ;; expansion! Just leave those alone (they will be removed anyway).
       (let ((uses (count-local-uses (map car defns) node)))
         (map (lambda (usecount defn)
                (when (and (= usecount 1)
                           (every? (lambda (t) (eq? t 'centring.lang/Any))
                                   (caddr defn)))
                  (hash-table-set! inlinables (car defn) (cdr defn))))
              uses defns)))
      (_)))

  (define (inline inlinables node)
    (match node
      (($ App ($ Local callee) args)
       (if (hash-table-exists? inlinables callee)
         (match-let (((formals _ body) (hash-table-ref inlinables callee)))
           (if (= (length formals) (length args))
             (let* ((f-as (alist->hash-table (map cons formals args)))
                    (body* (postwalk (cute replace-local-uses f-as <>) body)))
               body*)
             node))
         node))
      (_ node)))

  ;; need to run `remove-unuseds` immediately after this to keep the ast
  ;; 'alphatized':
  (define (beta-contract ast)
    (let ((inlinables (make-hash-table)))
      (prewalk (lambda (node)
                 (let ((node* (inline inlinables node)))
                   (add-inlinables! inlinables node*)
                   (inline inlinables node*)))
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
            (let* ((labels (map car defns))
                   (buses (count-local-uses labels body))
                   (uses (foldl
                          (lambda (uses defn)
                            (match-let (((label _ _ body) defn)
                                        (nuses (count-local-uses labels body)))
                              (map (lambda (l uc nuc)
                                     (if (eq? l label) uc (+ uc nuc)))
                                   labels uses nuses)))
                          buses defns))
                   (defns* (remove-udefns defns uses)))
              (if (null? defns*)
                body
                (make-Fix defns* body)))))
         ;; TODO: (($ Primop op args results conts) ...) ; only if fully pure
         ;; (halt is not pure, kills a VMProcess, iadd could overflow, ...)
         (_ node)))
     ast))

  ;;;; Closure Conversion

  (define (closure-convert locals clovers ast)
    (define (closure-convert-list locals clovers cexps res)
      (if (null? cexps)
        (list (reverse res) clovers)
        (match-let (((cexp* clovers*)
                     (closure-convert locals clovers (car cexps))))
          (closure-convert-list locals clovers* (cdr cexps) (cons cexp* res)))))

    (define (closure-convert-defn defn)
      (match-let* (((label formals types body) defn)
                   (locals (cons label formals))
                   ((body* clovers) (closure-convert locals '() body)))
        (list `(,label ,formals ,types ,body*) clovers)))
    (define (closure-convert-defns defns)
      (foldr (lambda (defn acc)
               (match-let (((defn* clovers) (closure-convert-defn defn)))
                 (list (cons defn* (car acc)) (cons clovers (cadr acc)))))
             (list '() '()) defns))
    
    (define ((replace-defnlabels labels closure-names) defn)
      (define (defnlabel-replacements labels closure-names repls)
        (cond
         ((null? labels) (alist->hash-table (reverse repls)))
         ((eq? (car labels) (car defn))
          (defnlabel-replacements (cdr labels) (cdr closure-names) repls))
         (else
          (defnlabel-replacements (cdr labels) (cdr closure-names)
            (cons (cons (car labels) (make-Local (car closure-names))) repls)))))
      
      (let ((replacements (defnlabel-replacements labels closure-names '())))
        (cast:update-defnbody
         (lambda (dbody)
           (postwalk (cute replace-local-uses replacements <>) dbody))
         defn)))
    
    (match ast
      (($ If cond then else)
       (match-let* (((cond* clovers*) (closure-convert locals clovers cond))
                    ((then* clovers**) (closure-convert locals clovers* then))
                    ((else* clovers***) (closure-convert locals clovers** else)))
         (list (make-If cond* then* else*) clovers***)))

      (($ Fix defns body)
       (match-let* ((labels (map car defns))
                    (closure-names (map gensym labels))
                    (defns* (map (replace-defnlabels labels closure-names)
                                 defns))
                    ((defns** clover-lls) (closure-convert-defns defns*))
                    (locals* (append locals closure-names))
                    (clovers* (foldl lset-append clovers clover-lls))
                    (body* (postwalk
                            (cute replace-local-uses
                                  (alist->hash-table
                                   (map (lambda (l c)
                                          (cons l (make-Local c)))
                                        labels closure-names))
                                  <>)
                            body))
                    ((body** clovers**) (closure-convert locals* clovers* body*)))
         (list (make-Fix defns**
                   (make-Close (map (lambda (name label clovers)
                                      `(,name ,label ,@(map make-Local clovers)))
                                    closure-names labels clover-lls)
                               body**))
               clovers*)))
      
      (($ Def name val cont)
       (match-let* (((val* clovers*) (closure-convert locals clovers val))
                    ((cont* clovers**) (closure-convert locals clovers* cont)))
         (list (make-Def name val* cont*) clovers**)))
      (($ Primop op args results conts)
       (match-let* (((args* clovers*)
                     (closure-convert-list locals clovers args '()))
                    (locals* (append locals results))
                    ((conts* clovers**)
                     (closure-convert-list locals* clovers* conts '())))
         (list (make-Primop op args* results conts*) clovers**)))
      (($ App callee args)
       (match-let* (((callee* clovers*) (closure-convert locals clovers callee))
                    ((args* clovers**)
                     (closure-convert-list locals clovers* args '())))
         (list (make-App callee* args*) clovers**)))
      
      (($ Local name)
       (if (member name locals)
         (list ast clovers)
         (let ((clovers* (lset-append clovers (list name))))
           (list (make-Clover (list-index (cute eq? name <>) clovers*))
                 clovers*))))
      
      ((or (? Const?) (? Global?)) (list ast clovers))))

  ;;;; Code Generation

  (define-record Procedure
    name
    (setter instrs)
    (setter consts)
    (setter procs)
    (setter cloverc)
    (setter local-names)
    (setter global-names))

  (define (Procedure->sexp proc)
    `(procedure
      (instructions ,@(Procedure-instrs proc))
      (constants ,@(Procedure-consts proc))
      (procedures ,@(Procedure-procs proc))
      (clover-count ,(Procedure-cloverc proc))

      (local-names ,@(Procedure-local-names proc))
      (global-names ,@(Procedure-global-names proc))))

  (define (push-instr! proc instr)
    (set! (Procedure-instrs proc)
          `(,@(Procedure-instrs proc) ,instr)))

  (define (push-constant! proc const)
    (set! (Procedure-consts proc)
          (lset-push (Procedure-consts proc) const))
    (list-index (cute eq? const <>) (Procedure-consts proc)))

  (define (update-clover-count! proc n)
    (set! (Procedure-cloverc proc)
          (max (Procedure-cloverc proc) n)))

  (define (local-index proc name)
    (list-index (cute eq? name <>) (Procedure-local-names proc)))

  (define (push-local! proc name)
    (set! (Procedure-local-names proc)
          (lset-push (Procedure-local-names proc) name)))

  (define (push-global! proc name)
    (set! (Procedure-global-names proc)
          (lset-push (Procedure-global-names proc) name))
    (list-index (cute eq? name <>) (Procedure-global-names proc)))

  (define (instr-arg! proc arg)
    (match arg
      (($ Local name) `(local ,(local-index proc name)))
      (($ Global name) `(global ,(push-global! proc name)))
      (($ Clover index)
       (update-clover-count! proc index)
       `(clover ,index))
      (($ Const val) `(const ,(push-constant! proc val)))))

  (define (instr-args! proc args)
    (map-in-order (cute instr-arg! proc <>) args))
  
  (define (emit! proc ast)
    (match ast
      (($ Primop op (arg1 arg2) (res) (cont))
       (push-instr! proc `(,op ,(instr-arg! proc arg1) ,(instr-arg! proc arg2)))
       (push-local! proc res)
       (emit! proc cont))
      (($ Primop 'halt (arg) '() '())
       (push-instr! proc `(halt ,(instr-arg! proc arg)))
       proc)))

  (define (emit ast)
    (let* ((main (gensym 'main))
           (proc (make-Procedure main '() '() '() 0 `(,main) '())))
      (Procedure->sexp (emit! proc ast)))))

(module centring.ast
  *
  
  (import scheme chicken)
  (use persistent-hash-map
       sequences
       matchable

       centring.util
       (only centring.primops op-purpose))

  ;;;; AST

  (define-record-type Fn
    (Fn arg cases ann)
    Fn?
    (arg Fn-arg)
    (cases Fn-cases)
    (ann Fn-ann))

  (define-record-type Primop
    (Primop op args conts ann)
    Primop?
    (op Primop-op)
    (args Primop-args)
    (conts Primop-conts)
    (ann Primop-ann))

  (define-record-type Fix
    (Fix bindings body ann)
    Fix?
    (bindings Fix-bindings)
    (body Fix-body)
    (ann Fix-ann))

  (define-record-type Do
    (Do stmts ann)
    Do?
    (stmts Do-stmts)
    (ann Do-ann))

  (define-record-type Const
    (Const val ann)
    Const?
    (val Const-val)
    (ann Const-ann))

  (define-record-type Global
    (Global res-ns ns name ann)
    Global?
    (res-ns Global-res-ns)
    (ns Global-ns)
    (name Global-name)
    (ann Global-ann))

  (define-record-type Local
    (Local name ann)
    Local?
    (name Local-name)
    (ann Local-ann))

  (define (ast->sexp ast)
    (match ast
      (($ Fn arg cases _)
       `($fn ,arg ,@(smap '() (cute smap '() ast->sexp <>) cases)))
      (($ Primop op args #f _)
       `(,(symbol-append '% op) ,@(smap '() ast->sexp args)))
      (($ Primop op args conts _)
       (case (op-purpose op)
         ((expr)
          (match-let ((#(($ Fn #(res) #(#(_ cbody)) _)) conts))
            `($let (,res (,(symbol-append '% op) ,@(smap '() ast->sexp args)))
               ,(ast->sexp cbody))))
         ((stmt)
          (match-let ((#(($ Fn #() #(#(_ cbody)) _)) conts))
            `($do (,(symbol-append '% op) ,@(smap '() ast->sexp args))
                  ,(ast->sexp cbody))))
         ((ctrl)
          (match-let ((#(($ Fn #() #(#(_ cbody)) _) ...) conts))
            `(,(symbol-append '% op) ,@(smap '() ast->sexp args)
              ,@(smap '() ast->sexp cbody))))))
      
      (($ Fix bindings body _)
       `($letrec ,(smap '() (match-lambda
                             ((var . expr) (cons var (ast->sexp expr))))
                        bindings)
                 ,(ast->sexp body)))
      (($ Do stmts _)
       `($do ,@(smap '() ast->sexp stmts)))
      (($ Const (and (? symbol?) val) _) `(quote ,val))
      (($ Const val _) val)
      (($ Global _ ns name _) (symbol-append (or ns '@@) ns-sep name))
      (($ Local name _) name)
      (_ (error "unable to display as S-expr" ast))))

  (define (node-map f node)
    (match node
      (($ Fn arg cases ann)
       (Fn arg (mapv (cute mapv f <>) cases) ann))
      (($ Primop op args conts ann)
       (Primop op (mapv f args) (if conts (mapv f conts) conts) ann))
      (($ Fix bindings body ann)
       (Fix (mapv (match-lambda ((var . expr) (cons var (f expr)))) bindings)
            (f body) ann))
      (($ Do stmts ann)
       (Do (mapv f stmts) ann))
      ((or (? Const?) (? Global?) (? Local?)) node)
      (_ (error "(node-map): not a valid node" node)))))
    

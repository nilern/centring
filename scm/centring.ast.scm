(module centring.ast
  *
  
  (import scheme chicken)
  (use persistent-hash-map
       matchable

       centring.util)

  ;;;; AST

  (define-record-type Fn
    (Fn arg cases ann)
    Fn?
    (arg Fn-arg)
    (cases Fn-cases)
    (ann Fn-ann))

  (define-record-type Primop
    (Primop op args ann)
    Primop?
    (op Primop-op)
    (args Primop-args)
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

  (define (node-map f node)
    (match node
      (($ Fn arg cases ann)
       (Fn arg (mapv (cute mapv f <>) cases) ann))
      (($ Primop op args ann)
       (Primop op (mapv f args) ann))
      (($ Fix bindings body ann)
       (Fix (mapv (match-lambda ((var . expr) (cons var (f expr)))) bindings)
            (f body) ann))
      (($ Do stmts ann)
       (Do (mapv f stmts) ann))
      ((or (? Const?) (? Global?) (? Local?)) node)
      (_ (error "(node-map): not a valid node" node)))))
    

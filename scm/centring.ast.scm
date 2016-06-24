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
    (Fn arg cases body)
    Fn?
    (arg Fn-arg)
    (cases Fn-cases)
    (body Fn-body))

  (define-record-type Primop
    (Primop op args conts)
    Primop?
    (op Primop-op)
    (args Primop-args)
    (conts Primop-conts))

  (define-record-type Fix
    (Fix bindings body)
    Fix?
    (bindings Fix-bindings)
    (body Fix-body))

  (define-record-type Do
    (Do stmts)
    Do?
    (stmts Do-stmts))

  (define-record-type Const
    (Const val)
    Const?
    (val Const-val))

  (define-record-type Global
    (Global res-ns ns name)
    Global?
    (res-ns Global-res-ns)
    (ns Global-ns)
    (name Global-name))

  (define-record-type Local
    (Local name)
    Local?
    (name Local-name))

  ;;;; Convert to S-expr

  (define (ast->sexp ast)
    (define (map-pl f p)
      (list (f (car p)) (f (cdr p))))
    
    (match ast
      (($ Fn (and (? symbol?) arg) cases _)
       `($fn ,arg ,@(smap '() (cute map-pl ast->sexp <>) cases)))
      (($ Fn arg cases _)
       `($fn ,(vector->list arg) ,@(smap '() (cute map-pl ast->sexp <>) cases)))
      (($ Primop op args #f)
       `(,(symbol-append '% op) ,@(smap '() ast->sexp args)))
      (($ Primop op args conts)
       (case (op-purpose op)
         ((expr)
          (match-let ((#(($ Fn #(res) #((_ . cbody)) _)) conts))
            `($let (,res (,(symbol-append '% op) ,@(smap '() ast->sexp args)))
               ,(ast->sexp cbody))))
         ((stmt)
          (match-let ((#(($ Fn #() #((_ . cbody)) _)) conts))
            `($do (,(symbol-append '% op) ,@(smap '() ast->sexp args))
                  ,(ast->sexp cbody))))
         ((ctrl)
          (match-let ((#(($ Fn #() #((_ . cbody)) _) ...) conts))
            `(,(symbol-append '% op) ,@(smap '() ast->sexp args)
              ,@(smap '() ast->sexp cbody))))))
      
      (($ Fix bindings body)
       `($letrec ,(smap '() (match-lambda
                             ((var . expr) (list var (ast->sexp expr))))
                        bindings)
                 ,(ast->sexp body)))
      (($ Do stmts)
       `($do ,@(smap '() ast->sexp stmts)))
      (($ Const (and (? symbol?) val)) `(quote ,val))
      (($ Const val) val)
      (($ Global _ ns name) (symbol-append (or ns '@@) ns-sep name))
      (($ Local name) name)
      (_ (error "unable to display as S-expr" ast))))

  ;;;; Traversal

  (define (node-map f node)
    (match node
      (($ Fn arg cases body)
       (Fn arg (mapv (cute map-pair f <>) cases) (if body (f body) body)))
      (($ Primop op args conts)
       (Primop op (mapv f args) (if conts (mapv f conts) conts)))
      (($ Fix bindings body)
       (Fix (mapv (match-lambda ((var . expr) (cons var (f expr)))) bindings)
            (f body)))
      (($ Do stmts)
       (Do (mapv f stmts)))
      ((or (? Const?) (? Global?) (? Local?)) node)
      (_ (error "(node-map): not a valid node" node)))))
    

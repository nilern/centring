(module centring.interpret
  *

  (import scheme chicken)
  (use matchable
       sequences
       (srfi 69)

       centring.ast
       centring.rt
       centring.primops
       centring.ns)

  ;;;;

  (define (eval-cps fiber node)
    (match node
      (($ Primop 'halt #(v) _)
       (eval-trivial fiber v))
      
      (($ Primop op args #(($ Fn #(resname) #(#(_ cbody)) _)))
       (let ((impl (Instr-impl (hash-table-ref primops op)))
             (argvals (smap #() (cute eval-trivial fiber <>) args)))
         (Fiber-local-set! fiber resname (impl fiber argvals))
         (eval-cps fiber cbody)))

      (($ Primop op args #(($ Fn #() #(#(_ cbody)) _)))
       (let ((impl (Instr-impl (hash-table-ref primops op)))
             (argvals (smap #() (cute eval-trivial fiber <>) args)))
         (impl fiber argvals)
         (eval-cps fiber cbody)))
      
      (_ (eval-trivial fiber node))))

  (define (eval-trivial fiber node)
    (match node
      (($ Const val) val)
      (($ Global res-ns ns-name name) (Fiber-global-ref fiber res-ns ns-name name))
      (($ Local name) (Fiber-local-ref fiber name))
      (_ (error "cannot eval" (ast->sexp node))))))
      

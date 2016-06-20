(module centring.interpret
  *

  (import scheme chicken)
  (use matchable
       sequences
       (srfi 69)

       centring.ast
       centring.primops
       centring.ns)

  ;;;;

  (define-record-type Fiber
    (Fiber locals)
    Fiber?
    (locals Fiber-locals))

  (define (make-fiber)
    (Fiber (make-hash-table)))

  (define (fiber-curr-ns fiber)
    (make-Ns 'centring.user #f #f #f))

  (define (fiber-local-ref fiber local)
    (hash-table-ref (Fiber-locals fiber) local))

  (define (fiber-local-set! fiber local v)
    (hash-table-set! (Fiber-locals fiber) local v))

  ;;;;

  (define (eval-cps fiber node)
    (match node
      (($ Primop 'halt #(v) _ _)
       (eval-trivial fiber v))
      
      (($ Primop op args #(($ Fn #(resname) #(#(_ cbody)) _)) _)
       (let ((impl (Instr-impl (hash-table-ref primops op)))
             (argvals (smap #() (cute eval-trivial fiber <>) args)))
         (fiber-local-set! fiber resname (impl fiber argvals))
         (eval-cps fiber cbody)))
      
      (_ (eval-trivial fiber node))))

  (define (eval-trivial fiber node)
    (match node
      (($ Const val _) val)
      (($ Local name _) (fiber-local-ref fiber name)))))
      

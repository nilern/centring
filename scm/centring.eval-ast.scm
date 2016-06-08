(module centring.eval-ast
  (make-interpreter eval-ast .ns-reg .curr-ns)

  (import scheme chicken)
  (use (srfi 69)
       sequences
       coops
       centring.analyze
       (prefix centring.ns ns:))

  ;;;;

  (define-class <interpreter> ()
    ((ns-reg :accessor .ns-reg)
     (curr-ns :accessor .curr-ns)))

  (define (make-interpreter)
    (let* ((ns-reg (make-hash-table))
           (centring.user (ns:ns-ref ns-reg 'centring.user)))
      (make <interpreter>
        'ns-reg ns-reg
        'curr-ns centring.user)))

  ;;;;

  (define-generic (eval-ast interpreter ast))

  (define-method (eval-ast (interpreter #t) (ast <const>))
    (.val ast))

  (define-method (eval-ast (interpreter #t) (ast <do>))
    ;; FIXME: use empty tuple as seed
    (foldl (lambda (_ stmt) (eval-ast interpreter stmt)) #t (.stmts ast)))

  (define-method (eval-ast (interpreter #t) (ast <primop>))
    (case (.op ast)
      ((set-ns!)
       (set! (.curr-ns interpreter)
             (ns:ns-ref (.ns-reg interpreter)
                        (.val (vector-ref (.args ast) 0))))
       #t)))) ; TODO: return empty tuple

(module centring.eval-ast
  (make-interpreter eval-ast .ns-reg .curr-ns)

  (import scheme chicken)
  (use (srfi 69)
       sequences
       coops
       (only clojurian-syntax ->>)

       (only centring.expand expand-all)
       centring.analyze
       (prefix centring.ns ns:)
       (only centring.rt read-ns))

  ;;;;

  (define-class <interpreter> ()
    ((ns-reg :accessor .ns-reg)
     (curr-ns :accessor .curr-ns)
     (path :accessor .path)))

  (define (make-interpreter path)
    (let* ((ns-reg (make-hash-table))
           (centring.user (ns:ns-ref ns-reg 'centring.user)))
      (make <interpreter>
        'ns-reg ns-reg
        'curr-ns centring.user
        'path path)))

  ;;;;

  (define-generic (eval-ast interpreter ast))

  (define-method (eval-ast (interpreter #t) (ast <const>))
    (.val ast))

  (define-method (eval-ast (itp #t) (ast <global>))
    ;; TODO: get resolution-ns from <global> to keep things lexical:
    (ns:lookup (.ns-reg itp) (.curr-ns itp) (.ns ast) (.name ast)))

  (define-method (eval-ast (interpreter #t) (ast <do>))
    ;; FIXME: use empty tuple as seed
    (foldl (lambda (_ stmt) (eval-ast interpreter stmt)) #t (.stmts ast)))

  (define-method (eval-ast (interpreter #t) (ast <primop>))
    ; TODO: make statements return empty tuples:
    (case (.op ast)
      ((set-ns!)
       (set! (.curr-ns interpreter)
             (ns:ns-ref (.ns-reg interpreter)
                        (.val (vector-ref (.args ast) 0))))
       #t)
      ((require!)
       (let ((ns-tmp (.curr-ns interpreter)))
         (->> (.val (vector-ref (.args ast) 0))
              (read-ns (.path interpreter))
              expand-all
              analyze
              (eval-ast interpreter))
         (set! (.curr-ns interpreter) ns-tmp)
         #t))
      ((set-global!)
       (ns:extend! (.curr-ns interpreter) (.val (vector-ref (.args ast) 0))
                   (eval-ast interpreter (vector-ref (.args ast) 1)))
       #t)
      (else (error "call to unimplemented intrinsic" (ast->sexp ast))))))

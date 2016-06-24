(module centring.cps
  *

  (import scheme chicken)
  (use matchable
       (srfi 1)
       (srfi 69)
       persistent-hash-map
       sequences
       (only miscmacros inc!)
       (only anaphora aif)
       (only extras pretty-print)

       centring.ast
       centring.util
       (only centring.analyze dnf)
       (prefix centring.primops ops:))

  ;;;; CPS Conversion

  (define (convert ast)
    (cps-k ast (lambda (v) (Primop 'halt (vector v) #()))))

  (define (cps-k ast c)
    (match ast
      ((? Fn?)
       (let ((f (gensym 'f)))
         (cps-k (Fix (vector (cons f ast)) (Local f)) c)))
      (($ Fix bindings body)
       (Fix (mapv (match-lambda ((var . f) (cons var (cps-fn f))))
                  bindings)
            (cps-k body c)))
      (($ Primop op args _)
       (cps-primop op args c))
      (($ Do stmts)
       (cps-stmts stmts c))
       
      ((? Const?) (c ast))
      ((? Global?) (c ast))
      ((? Local?) (c ast))

      (_ (error "unable to CPS convert" ast))))

  (define (cps-fn f)
    (match-let* ((($ Fn arg cases _) f)
                 (ret (gensym 'r)))
      (Fn (vector arg ret)
          (mapv (match-lambda
                 ((cond . body)
                  (cons
                   (cps-condition cond)
                   (cps-k body
                          (lambda (v)
                            (Primop 'call (vector (Local ret) v) #()))))))
                cases)
          #f)))

  (define (cps-primop op args c)
    (let ((purpose (ops:op-purpose op)))
      (case purpose
        ((expr)
         (cps-args
          args
          (lambda (as)
            (let ((res (gensym 'v)))
              (Primop op as
                      (vector
                       (Fn (vector res)
                           (vector
                            (cons
                             (cps-condition (dnf (Const #t)))
                             (c (Local res))))
                           #f)))))))
        ((stmt)
         (cps-args
          args
          (lambda (as)
            (Primop op as
                    (vector
                     (Fn #()
                         (vector
                          (cons
                           (cps-condition (dnf (Const #t)))
                           (cps-k
                            (Primop 'rec
                                    (vector (Global 'centring.lang
                                                    'centring.lang
                                                    'Tuple))
                                    #f)
                            c)))
                         #f))))))
        ((ctrl)
         (case op
           ((apply)
            (match-let ((#(callee arg) args))
              (cps-k
               callee
               (lambda (f)
                 (let* ((ret (gensym 'r))
                        (res (gensym 'v)))
                   (Fix (vector
                         (cons ret (Fn (vector res)
                                       (vector
                                        (cons
                                         (cps-condition (dnf (Const #t)))
                                         (c (Local res))))
                                       #f)))
                        (cps-k
                         arg
                         (lambda (a)
                           (Primop 'call (vector f a (Local ret)) #())))))))))
           ((halt)
            (match-let ((#(v) args))
              (Primop 'halt (vector v) #())))
           (else
            (error "unable to convert control operation" op))))
        (else (error "unable to convert primop with purpose" purpose)))))

  (define (cps-stmts stmts c)
    (match stmts
      (#()
       (cps-k
        (Primop 'rec
                (vector (Global 'centring.lang #f 'Tuple))
                #f)
        c))
      (#(stmt)
       (cps-k stmt c))
      (_
       (cps-k
        (foldl
         (lambda (acc stmt)
           (Primop 'call
                   (vector (Fn (gensym '_)
                               (vector (cons (dnf (Const #t)) stmt))
                               #f)
                           acc)
                   #f))
         (peek stmts)
         (pop stmts)) c))))

  (define (cps-args vec k)
    (let* ((len (vector-length vec))
           (res (make-vector len)))
      (define (cpsv i)
        (if (= i len)
          (k res)
          (cps-k (vector-ref vec i)
                 (lambda (v) (vector-set! res i v) (cpsv (add1 i))))))
      (cpsv 0)))

  (define (cps-condition cond)
    (define cps-or
      (match-lambda
       (($ Primop 'bior ands _)
        (Primop 'bior (mapv cps-and ands) #f))))
    (define cps-and
      (match-lambda
       (($ Primop 'band els _)
        (Primop 'band (mapv cps-el els) #f))))
    (define cps-el
      (match-lambda
       (($ Primop 'bnot #(at) _)
        (Primop 'bnot (vector (cps-el at)) #f))
       (at
        (cps-k at (lambda (v) (Primop 'yield (vector v) #()))))))
    (cps-or cond))

  ;;;; Utils

  (define (replace-sym rpls sym)
    (hash-table-ref/default rpls sym sym))
    
  ;;;; Analyses

  (define (census ast)
    (let ((symtab (make-symtab)))
      (define (census! ctx node)
        (match node
          (($ Fn args cases _)
           (doseq (arg args) ; add entries for argnames
             (add-id! symtab arg))
           (doseq (case cases)
             (match-let ((#(cond body) case))
               (census! ctx cond)
               (census! ctx body))))
          (($ Fix bindings body)
           (doseq (binding bindings) ; add entries for bound names
             (match-let (((var . expr) binding))
               (add-id! symtab var)))
           (doseq (binding bindings)
             (match-let (((var . expr) binding))
               (census! (push-fname ctx var) expr)))
           (census! ctx body))
          (($ Primop op (and args #(callee _ ...)) conts)
           (doseq (arg args)
             (census! ctx arg))
           (when conts
             (doseq (cont conts)
               (census! ctx cont)))
           (when (and (eq? op 'call) (Local? callee))
             (inc-callcount! symtab (Local-name callee))))
          
          (($ Local name)
           (inc-usecount! symtab name)
           (when (parent-scope? ctx name)
             (inc-rec-usecount! symtab name)))
           
          ((or (? Const?) (? Global?)))))
      (census! (make-ctx) ast)
      ast))

  (define make-symtab make-hash-table)
  (define (add-id! symtab name)
    (hash-table-set! symtab name (make-CensusEntry 0 0 0)))
  (define (inc-usecount! symtab name)
    (let ((entry (hash-table-ref symtab name)))
      (inc! (CensusEntry-usecount entry))))
  (define (inc-rec-usecount! symtab name)
    (let ((entry (hash-table-ref symtab name)))
      (inc! (CensusEntry-rec-usecount entry))))
  (define (inc-callcount! symtab name)
    (let ((entry (hash-table-ref symtab name)))
      (inc! (CensusEntry-callcount entry))))
  
  (define (make-ctx) '())
  (define (push-fname ctx name)
    (cons name ctx))
  (define (parent-scope? ctx name)
    (memq name ctx))

  (define-record CensusEntry
    (setter usecount)
    (setter rec-usecount)
    (setter callcount))

  (define-record-printer (CensusEntry entry out)
    (fprintf out "#,(CensusEntry ~S ~S ~S)"
             (CensusEntry-usecount entry)
             (CensusEntry-rec-usecount entry)
             (CensusEntry-callcount entry)))

  ;;;; Eta Contraction

  (define (eta-contract ast)
    (let ((eta-rpls (make-hash-table)))
      (define (etac! node)
        (match node
          (($ Fix bindings body)
           (let ((bindings*
                  (foldl
                   (match-lambda*
                    ((acc (var . (and ($ Fn args #((cond . body))) expr)))
                     (aif (and (tautology? cond)
                               (eta-replacing-name args body))
                       (begin
                         (hash-table-set! eta-rpls var (replace-sym eta-rpls it))
                         acc)
                       (cons (cons var (etac! expr)) acc)))
                    ((acc (var . expr))
                     (cons (cons var (etac! expr)) acc)))
                   '() bindings))
                 (body* (etac! body)))
             (if (null? bindings*)
               body*
               (Fix (reverse bindings*) body*))))
          (($ Local name)
           (Local (replace-sym eta-rpls name)))
          (_ (node-map etac! node))))
      (etac! ast)))

  (define (eta-replacing-name formals body)
    (define (maybe-Local-name node)
      (match node
        (($ Local name) name)
        (_ #f)))
    
    (match body
      (($ Primop 'call args _)
       (if (equal? (mapv maybe-Local-name (pop args)) formals)
         (Local-name (peek args))
         #f))
      (_ #f)))

  (define (tautology? cond)
    (match cond
      (($ Primop 'bior #(args ...) _)
       (any tautology? args))
      (($ Primop 'band #(args ...) _)
       (every tautology? args))
      (($ Primop 'bnot #(($ Primop yield #(($ Const #f)) _)) _)
       #t)
      (($ Primop 'yield #((? tautology?)) _)
       #t)
      (($ Const #t)
       #t)
      (_ #f))))

(library (ctr ast)
  (export make-Fn Fn? Fn-arg Fn-cases
          make-Primop Primop? Primop-op Primop-args Primop-conts
          make-Fix Fix? Fix-bindings Fix-body
          make-Do Do? Do-stmts
          make-Closure
          make-Global Global?
          Global-res-ns/var Global-res-ns/var-set! Global-ns Global-name
          make-Local Local? Local-name
          make-Const Const? Const-val
          node-map ast->sexp
          primop-case)
  (import (rnrs (6))

          (only (util) defrecord symbol-append if-let partial)
          (only (util collections) mapl)

          (only (ctr util) ctr-error literal? ns-sep))

  ;;;; AST

  (defrecord (Fn arg cases))
  (defrecord (Primop op args conts))
  (defrecord (Fix bindings body))
  (defrecord (Do stmts))
  (defrecord (Closure expr env))

  (define-record-type Global ;; FIXME: evolve defrecord macro to handle this
    (fields
     (mutable res-ns/var)
     (immutable ns)
     (immutable name)))
  (defrecord (Local name))
  (defrecord (Const val))

  ;;;;

  (define (node-map f node)
    (cond
     ((Fn? node)
      (make-Fn (Fn-arg node)
               (vector-map
                (lambda (case)
                  (cons (vector-map (partial vector-map f) (car case))
                        (f (cdr case))))
                (Fn-cases node))))
     ((Primop? node)
      (make-Primop (Primop-op node)
                   (vector-map f (Primop-args node))
                   (if-let (conts (Primop-conts node))
                       (vector-map f conts)
                       #f)))
     ((Fix? node)
      (make-Fix (vector-map (lambda (binding) (cons (car binding) (f (cdr binding))))
                            (Fix-bindings node))
                (f (Fix-body node))))
     ((Do? node)
      (make-Do (vector-map f (Do-stmts node))))
     ((Closure? node)
      (make-Closure (f (Closure-expr node)) (Closure-env node)))
     (else
      node)))   

  ;;;;

  (define (ast->sexp node)
    (cond
     ((Fn? node)
      `($fn ,(Fn-arg node)
            ,@(mapl (lambda (case)
                      `((%bior ,@(mapl (lambda (clause)
                                         `(%band ,@(mapl ast->sexp clause)))
                                       (car case)))
                        ,(ast->sexp (cdr case))))
                    (Fn-cases node))))
     ((Primop? node)
      `(,(symbol-append '% (Primop-op node))
        ,@(mapl ast->sexp (Primop-args node))
        ,@(if (Primop-conts node)
            (mapl ast->sexp (Primop-conts node))
            '())))
     ((Fix? node)
      `($letrec ,(mapl (lambda (binding)
                         (list (car binding) (ast->sexp (cdr binding))))
                       (Fix-bindings node))
                ,(ast->sexp (Fix-body node))))
     ((Do? node)
      `($do ,@(mapl ast->sexp (Do-stmts node))))
     ((Closure? node)
      `($closure ,(ast->sexp (Closure-expr node))))
     
     ((Global? node)
      (if-let (ns (Global-ns node))
        (symbol-append ns ns-sep (Global-name node))
        (Global-name node)))
     ((Local? node)
      (Local-name node))
     ((Const? node)
      (let ((val (Const-val node)))
        (if (symbol? val)
          `($quote ,val)
          val)))
     (else
      (ctr-error "unable to display as sexp" node))))

  ;;;;

  (define-syntax primop-case
    (syntax-rules (else)
      ((_ expr clauses ... (else default ...))
       (let ((val expr))
         (if (Primop? val)
           (case (Primop-op val) clauses ... (else default ...))
           (begin default ...)))))))

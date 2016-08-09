(library (ctr ast)
  (export make-Fn make-Primop make-Fix make-Do make-Closure make-Global make-Const
          ast->sexp)
  (import (rnrs (6))

          (only (util) defrecord symbol-append if-let)
          (only (util collections) mapl)

          (only (ctr util) ctr-error literal? ns-sep))

  ;;;; AST

  (defrecord (Fn arg cases))
  (defrecord (Primop op args conts))
  (defrecord (Fix bindings body))
  (defrecord (Do stmts))
  (defrecord (Closure expr env))
  
  (defrecord (Global res-ns ns name))
  (defrecord (Const val))

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
     ((Const? node)
      (let ((val (Const-val node)))
        (if (literal? val)
          val
          (ctr-error "unable to display Const containing" val))))
     (else
      (ctr-error "unable to display as sexp" node)))))

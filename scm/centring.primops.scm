(module centring.primops
  *
        
  (import scheme chicken)
  (use (srfi 69)
       matchable
       (only miscmacros define-syntax-rule)

       centring.util
       centring.value
       centring.env)

  ;;;;

  (define primops (make-hash-table))

  (defrecord (ExprOp impl))
  (defrecord (StmtOp impl))
  (defrecord (ScopeOp impl))
  (defrecord (CtrlOp impl))

  (define-syntax-rule (define-expression (name args ...) body ...)
    (define-primop name
      (ExprOp (lambda (argv)
                (match-let ((#(args ...) argv)) body ...)))))

  (define-syntax-rule (define-statement (name args ...) body ...)
    (define-primop name
      (StmtOp (lambda (argv)
                (match-let ((#(args ...) argv)) body ...)))))

  (define-syntax-rule (define-scopement (name env args ...) body ...)
    (define-primop name
      (ScopeOp (lambda (env argv)
                 (match-let ((#(args ...) argv)) body ...)))))

  (define-syntax-rule (define-controller (name conts args ...) body ...)
    (define-primop name
      (CtrlOp (lambda (conts argv)
                (match-let ((#(args ...) argv)) body ...)))))

  (define-syntax-rule (define-primop name val)
    (begin
      (define name val)
      (hash-table-set! primops (quote name) name)))

  ;;;;

  (define (op-purpose op-name)
    (match (hash-table-ref/default primops op-name #f)
     ((? ExprOp?) 'expr)
     ((? StmtOp?) 'stmt)
     ((? ScopeOp?) 'scope)
     ((? CtrlOp?) 'ctrl)
     (_ #f)))

  ;;;; Namespace Operations

  (define-scopement (set-global! env name val)
    (ns-extend! (Env-ns env) (Symbol-name name) val)
    env)

  ;;;; Arithmetic Operations

  ;;; TODO: detect overflow, div by zero:

  (define-expression (iadd a b)
    (fx+ a b))

  (define-expression (isub a b)
    (fx- a b))

  (define-expression (imul a b)
    (fx* a b))

  (define-expression (idiv a b)
    (fx/ a b))

  ;;;; Branches

  (define-controller (brf conts c)
    (if c
      (vector-ref conts 0)
      (vector-ref conts 1))))

;;   ;;;

;;   (define-primop rec _ argv --> ((cont d))
;;     argv)

;;   (define-primop rref _ (rec i) --> ((cont d) (throw Type) (throw Range))
;;     ;; TODO: better error messages:
;;     (vector-ref rec (add1 i)))

;;   (define-primop rset! _ (rec i v) -> ((cont) (throw Type) (throw Range))
;;     ;; TODO: better error messages:
;;     (vector-set! rec (add1 i) v))
  
;;   (define-primop rlen _ (rec) --> ((cont l) (throw Type) (throw Range))
;;     ;; TODO: better error messages:
;;     (sub1 (vector-length rec)))

;;   (define-primop type _ (v) --> ((cont d))
;;     ;; TODO: types of bits types (3, #f etc.):
;;     (vector-ref v 0))

;;   ;;;

;;   (define-primop bit-eq? _ (a b) --> ((cont d))
;;     (eq? a b))

(module centring.primops
  *
        
  (import scheme chicken)
  (use (srfi 69)
       matchable
       (only miscmacros define-syntax-rule)

       centring.value
       centring.env)

  ;;;;

  (define primops (make-hash-table))

  (define-record-type ExprOp
    (ExprOp impl)
    ExprOp?
    (impl ExprOp-impl))

  (define-record-type StmtOp
    (StmtOp impl)
    StmtOp?
    (impl StmtOp-impl))

  (define-syntax-rule (define-expression (name args ...) body ...)
    (begin
      (define name
        (ExprOp (match-lambda
                 (#(args ...) body ...))))
      (hash-table-set! primops (quote name) name)))

  (define-syntax-rule (define-statement (name env args ...) body ...)
    (begin
      (define name
        (StmtOp (lambda (env argv)
                  (match-let ((#(args ...) argv)) body ...))))
      (hash-table-set! primops (quote name) name)))

  ;;;;

  (define op-purpose
    (match-lambda
     ((? ExprOp) 'expr)
     ((? StmtOp) 'stmt)))

  ;;;;

  (define-statement (set-global! env name val)
    (ns-extend! (Env-ns env) (Symbol-name name) val)
    env)

  ;;;;

  ;;; TODO: detect overflow, div by zero:

  (define-expression (iadd a b)
    (fx+ a b))

  (define-expression (isub a b)
    (fx- a b))

  (define-expression (imul a b)
    (fx* a b))

  (define-expression (idiv a b)
    (fx/ a b)))

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

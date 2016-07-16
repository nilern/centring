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
  (defrecord (CtrlOp impl))

  (define-syntax-rule (define-expression (name args ...) body ...)
    (define-primop name
      (ExprOp (lambda (argv)
                (match-let ((#(args ...) argv)) body ...)))))

  (define-syntax-rule (define-statement (name args ...) body ...)
    (define-primop name
      (StmtOp (lambda (argv)
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
     ((? CtrlOp?) 'ctrl)
     (_ #f)))

  ;;;; Namespace Operations

  (define-statement (set-ns! ns-name)
    (current-ns (ns-ref (Symbol-name ns-name))))

  (define-statement (alias! ns-name as)
    (ns-alias! (current-ns)
               (ns-ref (Symbol-name ns-name))
               (Symbol-name as)))

  (define-statement (rename! var-name as)
    (ns-rename! (current-ns)
                (ns-ref (Symbol-ns var-name))
                (Symbol-name var-name)
                (Symbol-name as)))

  (define-statement (import! ns-name)
    (ns-import! (current-ns) (ns-ref (Symbol-name ns-name))))

  (define-statement (set-global! name val)
    (ns-extend! (current-ns) (Symbol-name name) val))

  ;;;; Type

  (define-expression (type v)
    (match v
      (#(t _ ...) t)
      (_ (error "(type) only implemented for records atm."))))

  ;;;; Records

  (define-primop rec
    (ExprOp (lambda (r) r)))

  (define-expression (rref r i)
    (vector-ref r (add1 i)))

  (define-statement (rset! r i v)
    (vector-set! r (add1 i) v))

  (define-expression (rlen r)
    (sub1 (vector-length r)))

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

;;   (define-primop bit-eq? _ (a b) --> ((cont d))
;;     (eq? a b))

(module centring.primops
  *
        
  (import scheme chicken)
  (use (srfi 69)
       matchable
       (only miscmacros define-syntax-rule)

       centring.util)

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
     (_ #f))))

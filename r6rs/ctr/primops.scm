(library (ctr primops)
  (export ExprOp? ExprOp-impl
          StmtOp? StmtOp-impl
          CtrlOp? CtrlOp-impl
          define-expression define-statement define-controller
          get-op op-name op-purpose)
  (import (rnrs (6))

          (only (util) defrecord)

          (only (ctr util) ctr-error))

  ;;; OPTIMIZE: destructuring could produce better code

  (define primops (make-eq-hashtable))

  (defrecord (ExprOp name impl))
  (defrecord (StmtOp name impl))
  (defrecord (CtrlOp name impl))

  (define-syntax define-primop
    (syntax-rules ()
      ((_ name val)
       (hashtable-set! primops (quote name) val))))

  (define-syntax define-expression
    (syntax-rules ()
      ((_ (name args ...) body ...)
       (define-primop name
         (make-ExprOp (quote name)
                      (vector-lambda (args ...) body ...))))))

  (define-syntax define-statement
    (syntax-rules ()
      ((_ (name args ...) body ...)
       (define-primop name
         (make-StmtOp (quote name)
                      (vector-lambda (args ...) body ...))))))

  (define-syntax define-controller
    (syntax-rules ()
      ((_ (name conts args ...) body ...)
       (define-primop name
         (make-CtrlOp (quote name)
                      (lambda (conts argv)
                        ((vector-lambda (args ...) body ...)
                         argv)))))))

  (define-syntax vector-lambda
    (syntax-rules ()
      ((_ (args ...) body ...)
       (lambda (vec)
         (vec-lambda-body vec 0 (args ...) body ...)))))

  (define-syntax vec-lambda-body
    (syntax-rules ()
      ((_ vec offset () body ...)
       (begin body ...))
      ((_ vec offset (arg args ...) body ...)
       (let ((arg (vector-ref vec offset)))
         (vec-lambda-body vec (+ offset 1) (args ...) body ...)))))

  ;;;;

  (define (get-op op-name)
    (hashtable-ref primops op-name #f))

  (define (op-purpose op-name)
    (let ((op (hashtable-ref primops op-name #f)))
      (cond
       ((ExprOp? op) 'expr)
       ((StmtOp? op) 'stmt)
       ((CtrlOp? op) 'ctrl)
       (else #f))))

  (define (op-name op)
    (cond
     ((ExprOp? op) (ExprOp-name op))
     ((StmtOp? op) (StmtOp-name op))
     ((CtrlOp? op) (CtrlOp-name op))
     (else (ctr-error "not a primop object" op)))))


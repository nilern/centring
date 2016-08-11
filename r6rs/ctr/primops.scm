(library (ctr primops)
  (export ExprOp? ExprOp-impl
          StmtOp? StmtOp-impl
          CtrlOp? CtrlOp-impl
          define-expression define-statement define-controller
          get-op op-purpose)
  (import (rnrs (6))

          (only (util) defrecord))

  ;;; OPTIMIZE: destructuring could produce better code

  (define primops (make-eq-hashtable))

  (defrecord (ExprOp impl))
  (defrecord (StmtOp impl))
  (defrecord (CtrlOp impl))

  (define-syntax define-primop
    (syntax-rules ()
      ((_ name val)
       (hashtable-set! primops (quote name) val))))

  (define-syntax define-expression
    (syntax-rules ()
      ((_ (name args ...) body ...)
       (define-primop name
         (make-ExprOp (vector-lambda (args ...) body ...))))))

  (define-syntax define-statement
    (syntax-rules ()
      ((_ (name args ...) body ...)
       (define-primop name
         (make-StmtOp (vector-lambda (args ...) body ...))))))

  (define-syntax define-controller
    (syntax-rules ()
      ((_ (name conts args ...) body ...)
       (define-primop name
         (make-CtrlOp (lambda (conts argv)
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
       (else #f)))))

(library (ctr primops)
  (export)
  (import (rnrs (6))

          (only (util) defrecord))

  (define primops (make-eq-hashtable))

  (defrecord (ExprOp impl))
  (defrecord (StmtOp impl))
  (defrecord (CtrlOp impl))

  ;;; TODO: need match-let:
  
  ;; (define-syntax-rule (define-expression (name args ...) body ...)
  ;;   (define-primop name
  ;;     (ExprOp (lambda (argv)
  ;;               (match-let ((#(args ...) argv)) body ...)))))

  ;; (define-syntax-rule (define-statement (name args ...) body ...)
  ;;   (define-primop name
  ;;     (StmtOp (lambda (argv)
  ;;               (match-let ((#(args ...) argv)) body ...)))))

  ;; (define-syntax-rule (define-controller (name conts args ...) body ...)
  ;;   (define-primop name
  ;;     (CtrlOp (lambda (conts argv)
  ;;               (match-let ((#(args ...) argv)) body ...)))))

  (define-syntax define-primop
    (syntax-rules ()
      ((define-primop name val)
       (begin
         (define name val)
         (hash-table-set! primops (quote name) name))))))

(library (ctr primop-impls)
  (export init!)
  (import (rnrs (6))

          (only (ctr primops) define-expression))

  (define (init!) ; HACK

  ;;;; Arithmetic Operations

  ;;; TODO: detect overflow, div by zero:

    (define-expression (iadd a b)
      (fx+ a b))

    (define-expression (isub a b)
      (fx- a b))

    (define-expression (imul a b)
      (fx* a b))

    (define-expression (idiv a b)
      (fxdiv a b))))

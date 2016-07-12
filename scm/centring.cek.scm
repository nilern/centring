(module centring.cek
  *

  (import scheme chicken)
  (use matchable
       (only clojurian-syntax doto)
       vector-lib
       (srfi 69)
       data-structures
       (only miscmacros until)

       centring.util
       centring.value
       centring.ast
       centring.env
       (only centring.primops primops))

  ;;;; Continuations

  (defrecord (Primop-cont op vals asts index conts cont))
  (defrecord (Do-cont args index cont))
  (defrecord (Halt-cont))

  ;;;; Machine

  (define (interpret ns-name ctrl)
    (define (run ctrl env k)
      (match ctrl
        ;; When ctrl is complex, start from first subexpr
        ;; and build a continuation:
        (($ Primop op args conts)
         (run (vector-ref args 0)
              env
              (Primop-cont op
                           (make-vector (vector-length args))
                           args
                           0
                           conts k)))
        (($ Do stmts)
         (run (vector-ref stmts 0) env (Do-cont stmts 0 k)))

        ;; When down to a constant, need to examine continuation:
        (($ Const v)
         (match k
           (($ Primop-cont op vals args i conts k)
            (let ((i* (add1 i))
                  (vals* (doto (vector-copy vals) (vector-set! i v))))
              (if (= i* (vector-length args))
                ;; perform operation:
                ;; TODO: embed *Instr in AST to remove the hash-ref:
                (match (hash-table-ref primops op)
                  (($ ExprOp impl)
                   (run (Const (impl vals*)) env k))
                  (($ StmtOp impl)
                   (impl vals*)
                   ;; TODO: empty tuple as ctrl:
                   (run (Const '()) env k))
                  (($ ScopeOp impl)
                   ;; TODO: empty tuple as ctrl:
                   (run (Const '()) (impl env vals*) k))
                  (($ CtrlOp impl)
                   (run (impl conts vals*) env k)))
                ;; evaluate next argument:
                (run (vector-ref args i*)
                     env
                     (Primop-cont op vals* args i* conts k)))))
           (($ Do-cont stmts i k)
            (if (= i (sub1 (vector-length stmts)))
              ;; last value gets passed to the continuation of the Do:
              (run ctrl env k)
              ;; throw value away and evaluate the next statement:
              (let ((i* (add1 i)))
                (run (vector-ref stmts i*) env (Do-cont stmts i* k)))))
           (($ Halt-cont)
            v)))

        ;; For Fns, build a closure:
        (($ Fn formal cases _)
         (run (Const (make-fn formal cases env)) env k))

        ;; For Symbols, look up the corresponding value:
        (($ Symbol ns name)
         (run (Const (env-lookup env ns name)) env k))))
    (run ctrl (make-env (ns-ref ns-name)) (Halt-cont))))

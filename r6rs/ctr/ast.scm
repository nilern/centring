(library (ctr ast)
  (export make-Fn make-Primop make-Fix make-Do make-Closure make-Const)
  (import (rnrs (6))

          (only (util) defrecord))

  ;;;; AST

  (defrecord (Fn arg cases body))
  (defrecord (Primop op args conts))
  (defrecord (Fix bindings body))
  (defrecord (Do stmts))
  (defrecord (Closure expr env ns))
  (defrecord (Const val)))

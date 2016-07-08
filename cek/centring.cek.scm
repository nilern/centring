(module centring.cek
  *

  (import scheme chicken)
  (use matchable
       (only clojurian-syntax doto)
       vector-lib
       (srfi 69)
       persistent-hash-map
       data-structures
       (only miscmacros until)

       centring.util
       centring.value
       centring.ast
       (only centring.primops primops Instr-impl))

  ;;;; State

  (define-record-type State
    (State ctrl env kont)
    State?
    (ctrl State-ctrl)
    (env State-env)
    (kont State-kont))

  ;;;; Continuations

  (define-record-type Halt-cont (Halt-cont) Halt-cont?)
  
  (define-record-type Primop-cont
    (Primop-cont op vals asts index cont)
    Primop-cont?
    (op Primop-cont-op)
    (vals Primop-cont-vals)
    (asts Primop-cont-asts)
    (index Primop-cont-index)
    (cont Primop-cont-cont))

  ;;;; Machine

  ;;; OPTIMIZE:
  ;;; * no explicit State, pass as args instead
  ;;; * no {Const, Local}-wrapping and -unwrapping

  (define (inject ctrl)
    (State ctrl (persistent-map) (Halt-cont)))

  (define extract (o Const-val State-ctrl))

  (define (step state)
    (match state
      ;; FnClosure:
      (($ State ($ Fn formal cases _) env k)
       (State (Const (make-fn formal cases env)) env k))

      ;; Primops:
      (($ State ($ Primop op args _) env k)
       (State (vector-ref args 0)
              env
              (Primop-cont op
                           (make-vector (vector-length args))
                           (mapv (cute cons <> env) args)
                           0 k)))
      (($ State ($ Const v) env ($ Primop-cont op vals args i k))
       (let ((i* (add1 i))
             (vals* (doto (vector-copy vals) (vector-set! i v))))
         (if (= i* (vector-length args))
           (let ((impl (Instr-impl (hash-table-ref primops op))))
             (State (Const (impl state vals*)) env k))
           (match-let (((arg . env) (vector-ref args i*)))
             (State arg env (Primop-cont op vals* args i* k))))))
      
      ;; Halt:
      (($ State ($ Const v) _ ($ Halt-cont))
       state)))

  (define (interpret state)
    (let ((state* (step state)))
      (if (eq? state* state)
        state
        (interpret state*)))))

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

  ;;;; Closures

  (define-record-type Closure
    (Closure expr env)
    Closure?
    (expr Closure-expr)
    (env Closure-env))

  ;;;; FnClosures

  (define-record-type FnClosure
    (FnClosure formal df cases caseq)
    FnClosure?
    (formal FnClosure-formal) ; symbol
    (df FnClosure-df)         ; DispatchNode
    (cases FnClosure-cases)   ; vector<#(AST, AST, Env)>
    (caseq FnClosure-caseq (setter FnClosure-caseq))) ; queue<#(AST, AST, Env)>

  (define (make-fn formal cases env)
    (let ((caseq (make-queue)))
      (doseq (case cases)
        (queue-add! caseq (vector (car case) (cdr case) env)))
      (FnClosure formal #f #() caseq)))

  (define (fn-merge! cl1 cl2)
    (match-let* ((($ FnClosure formal1 _ _ caseq1) cl1)
                 (($ FnClosure formal2 _ cases2 caseq2) cl2)
                 (caseq2* (make-queue)))
      (define (replace-formal v)
        (match v
          (($ Local (? (cute eq? formal1 <>))) (Local formal2))
          (_ v)))
      (define (replace-case-formal case)
        (match-let ((#(cond body env) case))
          (vector (postwalk replace-formal cond)
                  (postwalk replace-formal body)
                  env)))
      ;; cases already in use at cl2:
      (doseq (case cases2)
        (queue-add! caseq1 (replace-case-formal case)))
      ;; pending cases of cl2:
      (until (queue-empty? caseq2)
        (let ((case (queue-remove! caseq2)))
          (queue-add! caseq1 (replace-case-formal case))
          (queue-add! caseq2* case)))
      ;; undo the damage done to cl2:
      (set! (FnClosure-caseq cl2) caseq2*)))

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

  ;;;; API

  (define (interpret state)
    (let ((state* (step state)))
      (if (eq? state* state)
        state
        (interpret state*)))))

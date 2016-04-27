(module centring.emit
  *

  (import scheme chicken)
  (use (only (srfi 1) map-in-order)
       (only data-structures complement)
       (only matchable match match-lambda)
       (only clojurian-syntax doto)

       array
       (prefix centring.cps cps:))

  ;;;; Utils

  (define non-array? (complement array?))

  (define (arrset-push! arr v)
    (or (array-index (lambda (av) (equal? av v)) arr)
        (sub1 (array-push! arr v))))

  (define (multiarrset-combine mas1 mas2)
    (define (merge a b)
      (match (cons a b)
        (((? non-array?) . (? non-array?))
         (if (equal? a b) a (array a b)))
        (((? array?) . (? array?))
         (doto (array-clone a)
           (array-append! b)))
        (((? array?) . (? non-array?))
         (doto (array-clone a)
           (array-push! b)))
        (((? non-array?) . (? array?))
         (doto (make-array (add1 (array-length b)))
           (array-push! a)
           (array-append! b)))))
    (array-merge-with merge mas1 mas2))

  ;;;; Procedure Record to Hold Emission State

  (define-record-type Procedure
    (make-Procedure name formal-types
                    instrs consts procs cloverc local-names global-names)
    Procedure?

    (name Procedure-name)
    (formal-types Procedure-formal-types)
    (instrs Procedure-instrs)
    (consts Procedure-consts)
    (procs Procedure-procs)
    (cloverc Procedure-cloverc set-cloverc!)
    (local-names Procedure-local-names set-locals!)
    (global-names Procedure-global-names))

  (define (make-proc init-locals formal-types)
    (let ((name (car init-locals))
          (locals (list->array init-locals)))
      (make-Procedure name formal-types
                      (make-array 8) (make-array 8) (make-array 8) 0
                      locals (make-array 8))))

  ;;; convert to S-expr

  (define (Procedure->sexp proc)
    `(procedure ,(Procedure-name proc) ,(Procedure-formal-types proc)
      (procedures ,@(map Procedure->sexp
                         (array->list (Procedure-procs proc))))
      (instructions ,@(array->list (Procedure-instrs proc)))
      (constants ,@(array->list (Procedure-consts proc)))
      (clover-count ,(Procedure-cloverc proc))

      (local-names ,@(map (lambda (v) (if (array? v) (array->list v) v))
                          (array->list (Procedure-local-names proc))))
      (global-names ,@(array->list (Procedure-global-names proc)))))

  ;;; instructions

  (define (push-instr! proc instr)
    (array-push! (Procedure-instrs proc) instr))

  (define (set-instr! proc i instr)
    (array-set! (Procedure-instrs proc) i instr))

  ;;; constants

  (define (push-constant! proc const)
    (arrset-push! (Procedure-consts proc) const))

  ;;; subprocs

  (define (push-proc! proc subproc)
    (array-push! (Procedure-procs proc) subproc))

  (define (proc-index proc name)
    (array-index (match-lambda
                   (($ Procedure procname _ _ _ _ _ _) (eq? procname name)))
                 (Procedure-procs proc)))

  ;;; clover-count

  (define (update-clover-count! proc n)
    (set-cloverc! proc (max (Procedure-cloverc proc) (add1 n))))

  ;;; locals

  (define (local-index proc name)
    (define (pred v)
      (or (eq? v name)
          (and (array? v) (array-index (cute eq? name <>) v))))
    (array-index pred (Procedure-local-names proc)))

  (define (push-local! proc name)
    (array-push! (Procedure-local-names proc) name))

  (define (merge-locals! proc locals)
    (set-locals! proc (multiarrset-combine locals (Procedure-local-names proc))))

  ;;; globals

  (define (push-global! proc name)
    (arrset-push! (Procedure-global-names proc) name))

  ;;;; Emit

  (define (instr-arg! proc arg)
    (match arg
      (($ cps:Local name) `(local ,(local-index proc name)))
      (($ cps:Global name) `(global ,(push-global! proc name)))
      (($ cps:Clover index)
       (update-clover-count! proc index)
       `(clover ,index))
      (($ cps:Const val) `(const ,(push-constant! proc val)))))

  (define (instr-args! proc args)
    (map-in-order (cute instr-arg! proc <>) args))

  ;;;
  
  (define (emit! proc ast)
    (match ast
      (($ cps:If cond then else)
       (let ((brf-i (array-length (Procedure-instrs proc)))
             (cond-addr (instr-arg! proc cond))
             (oldlocals (array-clone (Procedure-local-names proc))))
         (doto proc
           (push-instr! `(nop)) ; placeholder
           (emit! then))
         (let ((else-i (array-length (Procedure-instrs proc)))
               (then-locals (Procedure-local-names proc)))
           (doto proc
             (set-locals! oldlocals) ; restore locals from start of if
             (emit! else)
             (merge-locals! then-locals) ; merge locals from both branches
             (set-instr! brf-i `(brf ,cond-addr ,else-i))))))
      (($ cps:Fix defns body)
       (for-each (match-lambda
                   ((name formals types body) ; TODO: emit the types
                    (push-proc! proc
                                (emit! (make-proc (cons name formals) types)
                                       body))))
                 defns)
       (emit! proc body))
      (($ cps:Close bindings body)
       (letrec ((emit-binding
                 (match-lambda
                   ((name label . clvs)
                    (for-each (lambda (clv)
                                (push-instr! proc `(load ,(instr-arg! proc clv))))
                              clvs)
                    (doto proc
                      (push-instr! `(fn ,(proc-index proc label)))
                      (push-local! name))))))
         (for-each emit-binding bindings)
         (emit! proc body)))
      (($ cps:Def name val cont)
       (doto proc
         (push-instr! `(def ,(push-global! proc name) ,(instr-arg! proc val)))
         (emit! cont)))
      (($ cps:Primop op (arg1 arg2) (res) (cont))
       (doto proc
         (push-instr! `(,op ,(instr-arg! proc arg1) ,(instr-arg! proc arg2)))
         (push-local! res)
         (emit! cont)))
      (($ cps:Primop op (arg1 arg2 arg3) '() (cont))
       (doto proc
         (push-instr! `(load ,(instr-arg! proc arg3)))
         (push-instr! `(,op ,(instr-arg! proc arg1) ,(instr-arg! proc arg2)))
         (emit! cont)))
      (($ cps:Primop op '() (res) (cont))
       (doto proc
         (push-instr! `(,op))
         (push-local! res)
         (emit! cont)))
      (($ cps:Primop 'halt (arg) '() '())
       (doto proc (push-instr! `(halt ,(instr-arg! proc arg)))))
      (($ cps:App callee args)
       (let ((callee-addr (array-length (Procedure-local-names proc))))
         (push-instr! proc `(load ,(instr-arg! proc callee)))
         (for-each (lambda (arg)
                     (push-instr! proc `(load ,(instr-arg! proc arg))))
                   args)
         (doto proc (push-instr! `(call ,callee-addr)))))
      (_ (error "don't know how to emit instructions for" ast))))

  (define (emit ast)
    (Procedure->sexp (emit! (make-proc (list (gensym 'main)) '()) ast))))

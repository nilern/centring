(module centring.emit
  *

  (import scheme chicken)
  (use (only (srfi 1) map-in-order)
       (only matchable match match-lambda)

       array
       (prefix centring.cps cps:))

  ;;;;

  (define (arrset-push! arr v)
    (or (array-index (lambda (av) (equal? av v)) arr)
        (sub1 (array-push! arr v))))

  (define-record Procedure
    name
    (setter instrs)
    (setter consts)
    (setter procs)
    (setter cloverc)
    (setter local-names)
    (setter global-names))

  (define (make-proc init-locals)
    (let ((name (car init-locals))
          (locals (list->array init-locals)))
      (make-Procedure name
                      (make-array 8) (make-array 8) (make-array 8) 0
                      locals (make-array 8))))

  (define (Procedure->sexp proc)
    `(procedure ,(Procedure-name proc)
      (procedures ,@(map Procedure->sexp
                         (array->list (Procedure-procs proc))))
      (instructions ,@(array->list (Procedure-instrs proc)))
      (constants ,@(array->list (Procedure-consts proc)))
      (clover-count ,(Procedure-cloverc proc))

      (local-names ,@(array->list (Procedure-local-names proc)))
      (global-names ,@(array->list (Procedure-global-names proc)))))

  (define (push-instr! proc instr)
    (array-push! (Procedure-instrs proc) instr))

  (define (push-constant! proc const)
    (arrset-push! (Procedure-consts proc) const))

  (define (push-proc! proc subproc)
    (array-push! (Procedure-procs proc) subproc))

  (define (proc-index proc name)
    (array-index (match-lambda
                   (($ Procedure procname _ _ _ _ _ _) (eq? procname name)))
                 (Procedure-procs proc)))

  (define (update-clover-count! proc n)
    (set! (Procedure-cloverc proc)
          (max (Procedure-cloverc proc) (add1 n))))

  (define (local-index proc name)
    (array-index (cute eq? name <>) (Procedure-local-names proc)))

  (define (push-local! proc name)
    (arrset-push! (Procedure-local-names proc) name))

  (define (push-global! proc name)
    (arrset-push! (Procedure-global-names proc) name))

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
  
  (define (emit! proc ast)
    (match ast
      (($ cps:If cond then else)
       (let ((brf-i (array-length (Procedure-instrs proc)))
             (cond-addr (instr-arg! proc cond)))
         (push-instr! proc `(nop))
         (emit! proc then)
         (let ((else-i (array-length (Procedure-instrs proc))))
           (emit! proc else)
           (array-set! (Procedure-instrs proc) brf-i `(brf ,cond-addr ,else-i))
           proc)))
      (($ cps:Fix defns body)
       (for-each (match-lambda
                   ((name formals _ body) ; TODO: emit the types
                    (push-proc! proc
                                (emit! (make-proc (cons name formals)) body))))
                 defns)
       (emit! proc body))
      (($ cps:Close ((name label . clvs)) body)
       (for-each (lambda (clv) (push-instr! proc `(load ,(instr-arg! proc clv))))
                 clvs)
       (push-instr! proc `(fn ,(proc-index proc label)))
       (push-local! proc name)
       (emit! proc body))
      (($ cps:Def name val cont)
       (push-instr! proc `(store ,(instr-arg! proc (cps:make-Global name))
                                 ,(instr-arg! proc val)))
       (emit! proc cont))
      (($ cps:Primop op (arg1 arg2) (res) (cont))
       (push-instr! proc `(,op ,(instr-arg! proc arg1) ,(instr-arg! proc arg2)))
       (push-local! proc res)
       (emit! proc cont))
      (($ cps:Primop op '() (res) (cont))
       (push-instr! proc `(,op))
       (push-local! proc res)
       (emit! proc cont))
      (($ cps:Primop 'halt (arg) '() '())
       (push-instr! proc `(halt ,(instr-arg! proc arg)))
       proc)
      (($ cps:App callee args)
       (let ((callee-addr (array-length (Procedure-local-names proc))))
         (push-instr! proc `(load ,(instr-arg! proc callee)))
         (for-each (lambda (arg)
                     (push-instr! proc `(load ,(instr-arg! proc arg))))
                   args)
         (push-instr! proc `(call ,callee-addr))
         proc))
      (_ (error "don't know how to emit instructions for" ast))))

  (define (emit ast)
    (Procedure->sexp (emit! (make-proc (list (gensym 'main))) ast))))

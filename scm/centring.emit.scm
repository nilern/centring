(module centring.emit
  *

  (import scheme chicken)
  (use matchable
       coops
       vector-lib
       sequences
       dyn-vector
       centring.cps
       centring.schring
       (prefix centring.value val:)
       (prefix centring.instructions instr:))

  ;;;

  (defrecord (ProcBuilder name instrs consts global-names local-names))

  (defrecord (Instr op args))

  (define (push-instr! proc instr)
    (dynvector-push! (.instrs proc) instr))

  (define (push-const! proc c)
    (dvset-push! (.consts proc) c))

  (define (push-local! proc name)
    (sub1 (dynvector-push! (.local-names proc) name)))

  (define (local-index proc name)
    (dynvector-index (cute eq? name <>) (.local-names proc)))

  ;;;

  (define (procb->sexpr proc)
    `(proc ,(.name proc)
           (instrs ,@(map instr->sexp (dynvector->list (.instrs proc))))
           (consts ,@(dynvector->list (.consts proc)))
           (global-names ,@(.global-names proc))
           (local-names ,@(dynvector->list (.local-names proc)))))

  (define (instr->sexp instr)
    `(,(.op instr) ,@(map arg->sexpr (vector->list (.args instr)))))

  (define (arg->sexpr arg)
    (match arg
      (($ Local i) `(local ,i))
      (($ Const i) `(const ,i))
      ((and (? fixnum?) i) i)))

  ;;;

  (define-generic (emit! proc node))

  (define-method (emit! (proc <ProcBuilder>) (node <Primop>))
    (match node
      (($ Primop 'halt #(v) #())
       (push-instr! proc (Instr 'halt (vector (emit-arg! proc v))))
       proc)
      (($ Primop op args #(($ Cont #(res) _ body)))
       (push-instr! proc
                    (Instr op (vector-append
                               (smap #() (cute emit-arg! proc <>) args)
                               (vector (push-local! proc res)))))
       (emit! proc body))))

  (define (emit-arg! proc arg)
    (match arg
      (($ Local name) (Local (local-index proc name)))
      (($ Const val) (Const (push-const! proc val)))))

  ;;;

  (define (assemble proc)
    (val:make-Proc
     (.name proc)
     (assemble-instrs (.instrs proc))
     (dynvector->vector (.consts proc))
     (.global-names proc)
     (.local-names proc)))

  (define (assemble-instrs instrs)
    (let ((res (make-dynvector 0 #f)))
      (dynvector-for-each
       (lambda (i instr)
         (dynvector-push! res (instr:instr-proc (.op instr)))
         (doseq (v (.args instr))
           (dynvector-push! res (assemble-arg v))))
       instrs)
      (dynvector->vector res)))

  (define (assemble-arg v)
    (match v
      (($ Local i) (bitwise-ior (arithmetic-shift i 3) 0))
      (($ Const i) (bitwise-ior (arithmetic-shift i 3) 2))
      ((and (? fixnum?) i) i))))

  

(module centring.vm
  *

  (import scheme chicken)
  (use (srfi 69)
       (only matchable match match-let)
       (only clojurian-syntax -> doto)
       (only miscmacros define-syntax-rule)
       (only extras printf fprintf)

       array
       (prefix centring.cps cps:))
  
  ;;;; Utils

  (define-syntax-rule (dolist (v ls) body ...)
    (for-each (lambda (v) body ...) ls))
  
  (define (arrset-push! arr v)
    (or (array-index (lambda (av) (equal? av v)) arr)
        (sub1 (array-push! arr v))))

  (define (multiarrset-combine mas1 mas2)
    (define (merge a b)
      (match (cons a b)
        (((not (? array?)) . (not (? array?)))
         (if (equal? a b) a (array a b)))
        (((? array?) . (? array?))
         (doto (array-clone a)
           (array-append! b)))
        (((? array?) . (not (? array?)))
         (doto (array-clone a)
           (array-push! b)))
        (((not (? array?)) . (? array?))
         (doto (make-array (add1 (array-length b)))
           (array-push! a)
           (array-append! b)))))
    (array-merge-with merge mas1 mas2))

  ;;;; Value

  (define-record gsymbol
    module
    name)

  (define-record proc
    name
    types
    instrs
    consts
    subprocs
    local-names
    global-names
    clover-count)

  (define-record-printer (proc tcode out)
    (doto out
      (fprintf "#<proc ~A ~A~%"
               (proc-name tcode) (proc-types tcode))
      (fprintf "  ~S~%" (proc-instrs tcode))
      (fprintf "  ~S~%" (proc-consts tcode))
      (fprintf "  ~S~%" (proc-subprocs tcode))
      (fprintf "  ~S>" (proc-local-names tcode))
      (fprintf "  ~S~%" (proc-global-names tcode))
      (fprintf "  ~S~%" (proc-clover-count tcode))))

  (define-record closure
    proc
    clovers)

  ;;;; Emit Threaded Code

  (define (push-const! tcode val)
    (arrset-push! (proc-consts tcode) val))

  ;;;

  (define (push-global! tcode name)
    (arrset-push! (proc-global-names tcode) name))

  ;;;

  (define (push-local! tcode name)
    (arrset-push! (proc-local-names tcode) name))

  (define (merge-locals! tcode locals)
    (proc-local-names-set! tcode
      (multiarrset-combine locals (proc-local-names tcode))))

  ;;;
  
  (define (subproc-index tcode name)
    (array-index (lambda (tcode) (eq? (proc-name tcode) name))
                 (proc-subprocs tcode)))

  ;;;

  (define (update-clover-count! tcode n)
    (let ((nc (max (proc-clover-count tcode) (add1 n))))
      (proc-clover-count-set! tcode nc)
      n))

  ;;;
  
  (define (encode-fetc-descr index tag)
    (-> index (arithmetic-shift 2) (bitwise-ior tag)))

  (define (emit-instr! tcode op)
    (array-push! (proc-instrs tcode) (hash-table-ref instr-lambdas op)))

  (define (emit-arg! tcode arg)
    (array-push! (proc-instrs tcode)
                 (match arg
                   (($ cps:Local name)
                    (encode-fetc-descr (push-local! tcode name) 0))
                   (($ cps:Clover i)
                    (encode-fetc-descr (update-clover-count! tcode i) 1))
                   (($ cps:Const val)
                    (encode-fetc-descr (push-const! tcode val) 2))
                   (($ cps:Global name)
                    (encode-fetc-descr (push-global! tcode name) 3)))))

  (define (emit! tcode cexp)
    (match cexp
      (($ cps:If cond then else)
       (doto tcode
         (emit-instr! 'brf)
         (emit-arg! cond))
       (let ((dest-i-loc (sub1 (array-push! (proc-instrs tcode) 0)))
             (oldlocals (array-clone (proc-local-names tcode))))
         (emit! tcode then)
         (let ((dest-i (array-length (proc-instrs tcode)))
               (then-locals (proc-local-names tcode)))
           (doto tcode
             (proc-local-names-set! oldlocals)
             (emit! else)
             (merge-locals! then-locals))
           (array-set! (proc-instrs tcode) dest-i-loc dest-i)
           tcode)))
      
      (($ cps:Fix defns body)
       (dolist (defn defns)
         (match-let (((label formals types body) defn))
           (array-push! (proc-subprocs tcode)
                        (emit label formals types body))))
       (emit! tcode body))
      
      (($ cps:Close bindings body)
       (dolist (binding bindings)
         (match-let (((name label . clvs) binding))
           (dolist (clv clvs)
             (emit-instr! tcode 'load)
             (emit-arg! tcode clv))
           (emit-instr! tcode 'fn)
           (array-push! (proc-instrs tcode) (subproc-index tcode label))
           (push-local! tcode name)))
       (emit! tcode body))
      
      (($ cps:Def name val cont)
       (emit-instr! tcode 'def)
       (array-push! (proc-instrs tcode) (push-global! tcode name))
       (doto tcode
         (emit-arg! val)
         (emit! cont)))
      
      (($ cps:Primop op args (res) (cont))
       (emit-instr! tcode op)
       (dolist (arg args)
         (emit-arg! tcode arg))
       (doto tcode
         (push-local! res)
         (emit! cont)))
      (($ cps:Primop 'halt (arg) '() '())
       (doto tcode
         (emit-instr! 'halt)
         (emit-arg! arg)))
      
      (($ cps:App callee args)
       (let ((callee-i (array-length (proc-local-names tcode))))
         (doto tcode
           (emit-instr! 'load)
           (emit-arg! callee))
         (dolist (arg args)
           (doto tcode
             (emit-instr! 'load)
             (emit-arg! arg)))
         (emit-instr! tcode 'call)
         (array-push! (proc-instrs tcode) callee-i)
         tcode))
      
      (_ (error "unable to emit code for" cexp))))

  ;; TODO: finally turn arrays into vectors
  (define (emit name formals types cexp)
    (let ((locals (list->array (cons name formals))))
      (emit! (make-proc name types
                                 (make-array 0) (make-array 0) (make-array 0)
                                 locals (make-array 0) 0)
             cexp)))
  
  ;;;; VM

  (define-record fiber
    curr-mod      ; HACK
    stack         ; ::array<value>
    sp            ; ::fixnum

    instrs        ; ::array
    ip            ; ::fixnum
    clovers       ; ::array<value>
    consts        ; ::array<value>
    subprocs      ; ::array<proc>
    global-names) ; ::array<symbol U gsymbol>

  ;;;

  (define (instr-peek fiber)
    (array-ref (fiber-instrs fiber) (fiber-ip fiber)))

  (define (instr-pop! fiber)
    (let ((v (instr-peek fiber)))
      (fiber-ip-set! fiber (add1 (fiber-ip fiber)))
      v))

  (define (fetch-instr! fiber)
    (instr-pop! fiber))

  (define (fetch-arg! fiber)
    (decode-fetch-descr fiber (instr-pop! fiber)))

  ;;;

  (define (local-ref fiber i)
    (array-ref (fiber-stack fiber) (+ (fiber-sp fiber) i)))

  (define (fiber-push! fiber v)
    (array-push! (fiber-stack fiber) v))

  ;;;

  (define (clover-ref fiber i)
    (array-ref (fiber-clovers fiber) i))

  ;;;

  (define (const-ref fiber i)
    (array-ref (fiber-consts fiber) i))

  ;;;

  (define (global-ref fiber i)
    (hash-table-ref (fiber-curr-mod fiber)
                    (array-ref (fiber-global-names fiber) i)))

  (define (global-set! fiber i v)
    (hash-table-set! (fiber-curr-mod fiber)
                     (array-ref (fiber-global-names fiber) i) v))

  ;;;

  (define (subproc-ref fiber i)
    (array-ref (fiber-subprocs fiber) i))

  ;;;

  (define (decode-fetch-descr fiber i)
    (case (bitwise-and i 3)
      ((0) (local-ref fiber (arithmetic-shift i -2)))
      ((1) (clover-ref fiber (arithmetic-shift i -2)))
      ((2) (const-ref fiber (arithmetic-shift i -2)))
      ((3) (global-ref fiber (arithmetic-shift i -2)))))

  ;;;

  (define (fetch-tcode! fiber tcode)
    (fiber-instrs-set! fiber (proc-instrs tcode))
    (fiber-consts-set! fiber (proc-consts tcode))
    (fiber-subprocs-set! fiber (proc-subprocs tcode))
    (fiber-global-names-set! fiber (proc-global-names tcode)))
  
  ;;;; Instructions

  (define instr-lambdas (make-hash-table))

  (define-syntax-rule (define-instruction (name fiber args ...) body ...)
    (hash-table-set! instr-lambdas (quote name)
                     (lambda (fiber)
                       (instruction-body (begin body ...) fiber args ...))))

  (define-syntax instruction-body
    (syntax-rules (fetch)
      ((_ body fiber (fetch arg) args ...)
       (let ((arg (fetch-arg! fiber)))
         (instruction-body body fiber args ...)))
      ((_ body fiber arg args ...)
       (let ((arg (fetch-instr! fiber)))
         (instruction-body body fiber args ...)))
      ((_ body fiber)
       (begin
         body
         (execute1! fiber)))))

  ;;; Loads

  (define-instruction (load fiber (fetch a))
    (fiber-push! fiber a))

  (define-instruction (void fiber)
    (fiber-push! fiber (cps:make-Unbound)))

  ;;; Module operations

  (define-instruction (def fiber i (fetch v))
    (global-set! fiber i v))

  ;;; Arithmetic

  (define-instruction (iadd fiber (fetch a) (fetch b))
    (fiber-push! fiber (+ a b)))

  (define-instruction (isub fiber (fetch a) (fetch b))
    (fiber-push! fiber (- a b)))

  (define-instruction (imul fiber (fetch a) (fetch b))
    (fiber-push! fiber (* a b)))

  (define-instruction (idiv fiber (fetch a) (fetch b))
    (fiber-push! fiber (quotient a b)))

  ;;; Functions

  (define-instruction (fn fiber i)
    (let* ((proc (subproc-ref fiber i))
           (clv-count (proc-clover-count proc))
           (clvs (array-split-off! (fiber-stack fiber) clv-count)))
      (fiber-push! fiber (make-closure proc clvs))))

  ;; TODO: recycle stack, handle calling other things etc.
  (define-instruction (call fiber i)
    (let* ((callee (local-ref fiber i))
           (proc (closure-proc callee)))
      (fetch-tcode! fiber proc)
      (fiber-clovers-set! fiber (closure-clovers callee))
      (fiber-ip-set! fiber 0)
      (fiber-sp-set! fiber (+ (fiber-sp fiber) i))))

  ;;; Control flow

  (define-instruction (brf fiber (fetch c) i)
    (unless c (fiber-ip-set! fiber i)))
  
  (hash-table-set! instr-lambdas 'halt fetch-arg!)

  ;;;; Interpret Threadcode

  (define (execute1! fiber)
    ((fetch-instr! fiber) fiber))

  (define (run! fiber tcode)
    (fetch-tcode! fiber tcode)
    (fiber-push! fiber tcode)
    (execute1! fiber)))
       
        

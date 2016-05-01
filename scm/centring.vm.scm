(module centring.vm
  *

  (import scheme chicken)
  (use (srfi 69)
       (only matchable match)
       (only clojurian-syntax -> doto)
       (only miscmacros define-syntax-rule)
       (only extras fprintf)

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

  (define-record threaded-code
    instrs
    consts
    local-names)

  (define-record-printer (threaded-code tcode out)
    (doto out
      (fprintf "#<threaded-code~%")
      (fprintf "  ~S~%" (threaded-code-instrs tcode))
      (fprintf "  ~S~%" (threaded-code-consts tcode))
      (fprintf "  ~S>" (threaded-code-local-names tcode))))

  ;;;; Emit Threaded Code

  (define (emit-instr! tcode op)
    (array-push! (threaded-code-instrs tcode) (hash-table-ref instr-lambdas op)))

  (define (push-const! tcode val)
    (arrset-push! (threaded-code-consts tcode) val))

  (define (tc-push-local! tcode name)
    (arrset-push! (threaded-code-local-names tcode) name))

  (define (merge-locals! tcode locals)
    (threaded-code-local-names-set! tcode
      (multiarrset-combine locals (threaded-code-local-names tcode))))

  (define (encode-load index tag)
    (-> index (arithmetic-shift 2) (bitwise-ior tag)))

  (define (emit-arg! tcode arg)
    (array-push! (threaded-code-instrs tcode)
                 (match arg
                   (($ cps:Local name) (encode-load (tc-push-local! tcode name) 0))
                   (($ cps:Const val) (encode-load (push-const! tcode val) 2)))))

  (define (emit! tcode cexp)
    (match cexp
      (($ cps:If cond then else)
       (doto tcode
         (emit-instr! 'brf)
         (emit-arg! cond))
       (let ((dest-i-loc (sub1 (array-push! (threaded-code-instrs tcode) 0)))
             (oldlocals (array-clone (threaded-code-local-names tcode))))
         (emit! tcode then)
         (let ((dest-i (array-length (threaded-code-instrs tcode)))
               (then-locals (threaded-code-local-names tcode)))
           (doto tcode
             (threaded-code-local-names-set! oldlocals)
             (emit! else)
             (merge-locals! then-locals))
           (array-set! (threaded-code-instrs tcode) dest-i-loc dest-i)
           tcode)))
      (($ cps:Primop op (arg1 arg2) (res) (cont))
       (doto tcode
         (emit-instr! op)
         (emit-arg! arg1)
         (emit-arg! arg2)
         (tc-push-local! res)
         (emit! cont)))
      (($ cps:Primop 'halt (arg) '() '())
       (doto tcode
         (emit-instr! 'halt)
         (emit-arg! arg)))))

  (define (emit cexp)
    (emit! (make-threaded-code (make-array 2) (make-array 2) (make-array 2))
           cexp))
  
  ;;;; VM

  (define-record fiber
    stack         ; ::array<value>
    sp            ; ::fixnum

    tcode         ; ::threaded-code
    ip)           ; ::fixnum

  ;;;

  (define (current-instr fiber)
    (array-ref (threaded-code-instrs (fiber-tcode fiber)) (fiber-ip fiber)))

  (define (read-instr! fiber)
    (let ((v (current-instr fiber)))
      (fiber-ip-set! fiber (add1 (fiber-ip fiber)))
      v))

  (define (load-instr! fiber)
    (read-instr! fiber))

  (define (load-arg! fiber)
    (decode-load fiber (read-instr! fiber)))

  ;;;

  (define (local-ref fiber i)
    (array-ref (fiber-stack fiber) (+ (fiber-sp fiber) i)))

  (define (push-local! fiber v)
    (array-push! (fiber-stack fiber) v))

  ;;;

  (define (const-ref fiber i)
    (array-ref (threaded-code-consts (fiber-tcode fiber)) i))

  ;;;

  (define (decode-load fiber i)
    (case (bitwise-and i 3)
      ((0) (local-ref fiber (arithmetic-shift i -2)))
      ;; ((1) (clover-ref fiber (arithmetic-shift i -2)))
      ((2) (const-ref fiber (arithmetic-shift i -2)))
      #|((3) (global-ref fiber (arithmetic-shift i -2)))|#))
  
  ;;;; Instructions

  (define instr-lambdas (make-hash-table))

  (define-syntax-rule (define-instruction (name fiber params ...) body ...)
    (hash-table-set! instr-lambdas (quote name)
                     (lambda (fiber)
                       (let ((params (load-arg! fiber)) ...)
                         body ...
                         (execute1! fiber)))))

  (define-instruction (iadd fiber a b)
    (push-local! fiber (+ a b)))

  (define-instruction (isub fiber a b)
    (push-local! fiber (- a b)))

  (define-instruction (imul fiber a b)
    (push-local! fiber (* a b)))

  (define-instruction (idiv fiber a b)
    (push-local! fiber (quotient a b)))

  (define-instruction (brf fiber c i)
    (unless c (fiber-ip-set! fiber i)))
  
  (hash-table-set! instr-lambdas 'halt load-arg!)

  ;;;; Interpret Threadcode

  (define (execute1! fiber)
    ((load-instr! fiber) fiber))

  (define (run! fiber)
    (execute1! fiber)))
       
        

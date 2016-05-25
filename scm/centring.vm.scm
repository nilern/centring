(module centring.vm
  *

  (import scheme chicken)
  (use dyn-vector
       centring.value)

  ;;;; Fibers

  (define-record fiber
    aregs
    pregs
    
    instrs
    ip

    consts)

  ;;; instrs

  (define (instr-peek fiber)
    (vector-ref (fiber-instrs fiber) (fiber-ip fiber)))

  (define (instr-pop! fiber)
    (let ((v (instr-peek fiber)))
      (fiber-ip-set! fiber (add1 (fiber-ip fiber)))
      v))

  (define (fetch-instr! fiber)
    (instr-pop! fiber))

  (define (fetch-arg! fiber)
    (decode-fetch-descr fiber (instr-pop! fiber)))

  ;;; decode

  (define (decode-fetch-descr fiber i)
    (case (bitwise-and i 3)
      ((0) (local-ref fiber (arithmetic-shift i -3)))
      ;; ((1) (clover-ref fiber (arithmetic-shift i -2)))
      ((2) (const-ref fiber (arithmetic-shift i -3)))
      ;; ((3) (global-ref fiber (arithmetic-shift i -2)))
      ))

  ;;; locals

  (define (local-ref fiber i)
    (dynvector-ref (fiber-aregs fiber) i))

  (define (local-set! fiber i v)
    (dynvector-set! (fiber-aregs fiber) i v))

  ;;; constants

  (define (const-ref fiber i)
    (vector-ref (fiber-consts fiber) i))

  ;;;; Interpret Threadcode

  (define (run! fiber proc)
    (fiber-consts-set! fiber (Proc-consts proc))
    
    (fiber-instrs-set! fiber (Proc-instrs proc))
    (fiber-ip-set! fiber 0)
    (execute-1! fiber))

  (define (execute-1! fiber)
    ((fetch-instr! fiber) fiber)))

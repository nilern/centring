(module centring.vm
  *

  (import scheme chicken)

  ;;;; Fibers

  (define-record fiber
    aregs
    pregs
    
    instrs
    ip)

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
      ((0) (local-ref fiber (arithmetic-shift i -2)))
      ;; ((1) (clover-ref fiber (arithmetic-shift i -2)))
      ;; ((2) (const-ref fiber (arithmetic-shift i -2)))
      ;; ((3) (global-ref fiber (arithmetic-shift i -2)))
      ))

  ;;; locals

  (define (local-ref fiber i)
    (vector-ref (fiber-aregs fiber) i))

  (define (local-set! fiber i v)
    (vector-set! (fiber-aregs fiber) i v))

  ;;;; Interpret Threadcode

  (define (execute-1! fiber)
    ((fetch-instr! fiber) fiber)))

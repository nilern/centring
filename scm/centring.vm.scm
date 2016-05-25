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

  (define (fetch-arg*! fiber dest)
    (define (push! v)
      (dynvector-set! dest (dynvector-length dest) v))
    (define (append-tup! tup)
      (let ((len (vector-length tup)))
        (do ((i 1 (add1 i))) ((= i len))
          (push! (vector-ref tup i)))))
    
    (let ((i (instr-pop! fiber)))
      (case (bitwise-and i 3)
        ((0) (push! (local-ref fiber (arithmetic-shift i -3))))
       ; ((1)
        ((2) (push! (const-ref fiber (arithmetic-shift i -3))))
       ; ((3)
        ((4) (append-tup! (local-ref fiber (arithmetic-shift i -3))))
       ; ((5)
        ((6) (append-tup! (const-ref fiber (arithmetic-shift i -3)))))))
       ; ((7)

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

  (define (swap-regs! fiber)
    (let ((tmp (fiber-aregs fiber)))
      (fiber-aregs-set! fiber (fiber-pregs fiber))
      (fiber-pregs-set! fiber tmp)))

  ;;; constants

  (define (const-ref fiber i)
    (vector-ref (fiber-consts fiber) i))

  ;;;; Interpret Threadcode

  ;; HACK: this overlaps with the call instruction:
  (define (run! fiber proc)
    (fiber-consts-set! fiber (Proc-consts proc))
    
    (fiber-instrs-set! fiber (Proc-instrs proc))
    (fiber-ip-set! fiber 0)
    (execute-1! fiber))

  (define (execute-1! fiber)
    ((fetch-instr! fiber) fiber)))

(module centring.vm
  *

  (import scheme chicken)
  (use (srfi 69)

       array)

  (define-record fiber
    curr-mod
    stack
    sp

    instrs
    ip
    consts
    clovers
    subprocs
    global-names)

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

  (define (local-set! fiber i v)
    (array-set! (fiber-stack fiber) (+ (fiber-sp fiber) i)))

  (define (fiber-push! fiber v)
    (array-push! (fiber-stack fiber) v))

  (define (fiber-pop! fiber)
    (array-pop! (fiber-stack fiber)))

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

  (define (decode-fetch-descr fiber i)
    (case (bitwise-and i 3)
      ((0) (local-ref fiber (arithmetic-shift i -2)))
      ((1) (clover-ref fiber (arithmetic-shift i -2)))
      ((2) (const-ref fiber (arithmetic-shift i -2)))
      ((3) (global-ref fiber (arithmetic-shift i -2)))))

  ;;;; Interpret Threadcode

  (define (execute-1! fiber)
    ((fetch-instr! fiber) fiber)))

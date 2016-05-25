(module centring.instructions
  *

  (import scheme chicken)
  (use (srfi 69)
       matchable

       centring.schring
       (prefix centring.vm vm:))

  ;;;; Instruction Type and Table

  (defrecord (Instruction arg-descrs cont-descrs elidable code))
  
  (define instructions (make-hash-table))

  ;;;; Instruction-definition Macro

  (define-syntax define-instruction
    (syntax-rules (lambda)
      ((_ name arg-descrs arrow conts (lambda (fiber . args) body ...))
       (begin
         ;; implementation:
         (define (name fiber)
           (instruction-body fiber arg-descrs args conts (body ...)))
         ;; store Instruction in the instruction table:
         (hash-table-set! instructions (quote name)
                          (instruction-construction
                           name arg-descrs arrow conts))))))

  (define-syntax instruction-construction
    (syntax-rules (-> -->)
      ;; trivially elidable:
      ((_ name arg-descrs --> conts)
       (Instruction (quote arg-descrs) (quote conts) #t name))
      ;; not (unconditionally) elidable:
      ((_ name arg-descrs -> conts)
       (Instruction (quote arg-descrs) (quote conts) #f name))))

  (define-syntax instruction-body
    (syntax-rules (fd index cont)
      ;; Fetching an argument (no splats):
      ((_ fiber (fd adescrs ...) (arg . args) conts body)
       (let ((arg (vm:fetch-arg! fiber)))
         (instruction-body fiber (adescrs ...) args conts body)))
      ;; Fetching some index (directly from instruction stream):
      ((_ fiber (index adescrs ...) (arg . args) conts body)
       (let ((arg (vm:fetch-instr! fiber)))
         (instruction-body fiber (adescrs ...) args conts body)))
      
      ;; Produces a value in register? Fetch the index of the dest register:
      ((_ fiber () (dest) ((cont _)) body)
       (let ((dest (vm:fetch-instr! fiber)))
         (instruction-body fiber () () () body)))
      ;; A bidirectional conditional branch? Just need the body:
      ((_ fiber () () ((cont) (cont)) body)
       (instruction-body fiber () () () body))
      ;; Halt? Eval body and return value:
      ((_ fiber () () #f (body ...))
       (begin body ...))

      ;; Splice in the body and dispatch next instruction:
      ((_ fiber () () () (body ...))
       (begin
         body ...
         (vm:execute-1! fiber)))))

  ;;;; Instructions

  ;;; Arithmetic

  (define-instruction iadd
    (fd fd) -> ((cont Int))
    (lambda (fiber a b d)
      (vm:local-set! fiber d (fx+ a b))))

  (define-instruction isub
    (fd fd) -> ((cont Int))
    (lambda (fiber a b d)
      (vm:local-set! fiber d (fx- a b))))

  (define-instruction imul
    (fd fd) -> ((cont Int))
    (lambda (fiber a b d)
      (vm:local-set! fiber d (fx* a b))))

  (define-instruction idiv
    (fd fd) -> ((cont Int))
    (lambda (fiber a b d)
      (vm:local-set! fiber d (fx/ a b))))

  ;;; Control Flow

  (define-instruction brf
    (fd index) --> ((cont) (cont))
    (lambda (fiber c ip*)
      (unless c
        (vm:fiber-ip-set! fiber ip*))))

  (define-instruction halt
    (fd) -> #f
    (lambda (fiber res)
      res))

  ;;; Functions

  ;; (define-instruction call
  ;;   (fd . fd*) -> ()
  ;;   (lambda (fiber f n)
  
  ;;;; Query Instruction Table

  (define (instr-proc op)
    (.code (hash-table-ref instructions op)))

  (define (produces-result? op)
    (match (.cont-descrs (hash-table-ref instructions op))
      ((('cont _)) #t)
      (_ #f)))

  (define (result-type op)
    (match (.cont-descrs (hash-table-ref instructions op))
      ((('cont type)) (symbol-append 'centring.lang/ type))
      (_ (error "produces no result of any type" op)))))

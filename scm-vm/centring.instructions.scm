(module centring.instructions
  *

  (import scheme chicken)
  (use (srfi 69)
       (only matchable match match-let)

       (only centring.util stack-push!)
       (prefix centring.vm vm:))

  (use-for-syntax (only matchable match match-let)
                  (only sequences thereis?))

  ;;;; Instruction Type and Operations

  (define-record instruction
    arg-descrs
    cont-descrs
    elidable
    code)
  
  (define instructions (make-hash-table))

  (define (valid-intrinsic? op args)
    (and (instr-name? op)
         (valid-arity? op (length args))))

  (define (instr-name? sym)
    (hash-table-exists? instructions sym))

  (define (valid-arity? sym n)
    (= n (length (instruction-arg-descrs (hash-table-ref instructions sym)))))

  (define (side-effecting? op)
    (memq op '(iadd isub imul idiv set-global!)))

  ;;;; Instruction-definition Macro

  (define-syntax define-instruction
    (ir-macro-transformer
     (lambda (form _ compare?)
       (match-let (((_ name arg-descrs arrow conts (__ (fiber . args) . body))
                    form))
         `(begin
            (define (,name ,fiber)
              (instruction-body ,fiber ,arg-descrs ,args
                                ,(thereis? (lambda (v) (compare? (car v) 'cont))
                                           conts)
                                ,body))
            (hash-table-set!
             instructions (quote ,name)
             (make-instruction (quote ,arg-descrs)
                               (quote ,conts)
                               ,(cond
                                 ((compare? arrow '->) #f)
                                 ((compare? arrow '-->) #t)
                                 (else (error "invalid arrow" arrow)))
                               ,name)))))))

  (define-syntax instruction-body
    (ir-macro-transformer
     (lambda (form _ compare?)
       (let ((fd? (lambda (v) compare? v 'fd))
             (index? (lambda (v) compare? v 'index)))
         (match form
           ((_ fiber (adescr . adescrs) (arg . args) continue? body)
            `(let ((,arg (,(cond
                            ((fd? adescr) 'vm:fetch-arg!)
                            ((index? adescr) 'vm:fetch-instr!)
                            (else (error "invalid instruction arg-descr" adescr)))
                           ,fiber)))
               (instruction-body ,fiber ,adescrs ,args ,continue? ,body)))
           ((_ fiber '() '() #t body)
            `(begin ,@body (vm:execute-1! ,fiber)))
           ((_ fiber '() '() #f body)
            `(begin ,@body))
           (_ (error "invalid instruction-body call" form)))))))

  ;;;; Instructions

  ;;; Namespace Management

  (define-instruction set-global!
    (index fd) -> ((cont))
    (lambda (fiber i v)
      (vm:global-set! fiber i v)))

  ;;; Records

  ;; (define-instruction record
  ;;   fd* --> ((cont Any))
  ;;   (lambda (fiber as)
  ;;     (vm:fiber-push! fiber as)))

  (define-instruction block-ref
    (fd fd) -> ((cont Any) (throw OutOfBounds))
    (lambda (fiber rec i)
      (vm:fiber-push! fiber (vector-ref rec i))))

  (define-instruction block-set!
    (fd fd fd) -> ((throw OutOfBounds) (throw Immutable))
    (lambda (fiber rec i v)
      (vector-set! rec i v)))

  ;;; Arithmetic

  (define-instruction iadd
    (fd fd) -> ((cont Int) (throw Overflow))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (fx+ a b))))

  (define-instruction isub
    (fd fd) -> ((cont Int) (throw Overflow))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (fx- a b))))

  (define-instruction imul
    (fd fd) -> ((cont Int) (throw Overflow))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (fx* a b))))

  (define-instruction idiv
    (fd fd) -> ((cont Int) (throw Overflow) (throw DivideByZero))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (fx/ a b))))

  (define-instruction irem
    (fd fd) --> ((cont Int))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (remainder a b))))

  (define-instruction imod
    (fd fd) --> ((cont Int))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (modulo a b))))

  ;;; Bit Operations

  (define-instruction iand
    (fd fd) --> ((cont Int))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (bitwise-and a b))))

  (define-instruction ior
    (fd fd) --> ((cont Int))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (bitwise-ior a b))))

  (define-instruction ixor
    (fd fd) --> ((cont Int))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (bitwise-xor a b))))

  (define-instruction inot 
    (fd) --> ((cont Int))
    (lambda (fiber a)
      (vm:fiber-push! fiber (bitwise-not a))))

  (define-instruction iash
    (fd fd) --> ((cont Int))
    (lambda (fiber a b)
      (vm:fiber-push! fiber (arithmetic-shift a b))))

  ;;; Control Flow

  (define-instruction brf
    (fd index) --> ((cont) (cont))
    (lambda (fiber c i)
      (unless c (vm:fiber-ip-set! fiber i))))

  (define-instruction halt
    (fd) -> ()
    (lambda (fiber v)
      (vm:fiber-push! fiber v))))

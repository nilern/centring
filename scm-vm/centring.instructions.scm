(module centring.instructions
  *

  (import scheme chicken)
  (use (srfi 69)
       (only matchable match match-let)

       (only centring.util stack-push!)
       (only centring.vm fiber-stack execute-1! fetch-arg! fetch-instr!))

  (use-for-syntax (only matchable match match-let))

  ;;;

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

  (define-syntax define-instruction
    (ir-macro-transformer
     (lambda (form _ compare?)
       (match-let (((_ name arg-descrs arrow conts (__ (fiber . args) . body))
                    form))
         `(begin
            (define (,name ,fiber)
              (instruction-body ,fiber ,arg-descrs ,args ,body))
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
           ((_ fiber (adescr . adescrs) (arg . args) body)
            `(let ((,arg (,(cond
                            ((fd? adescr) 'fetch-arg!)
                            ((index? adescr) 'fetch-instr!)
                            (else (error "invalid instruction arg-descr" adescr)))
                           ,fiber)))
               (instruction-body ,fiber ,adescrs ,args ,body)))
           ((_ fiber '() '() body)
            `(begin ,@body (execute-1! ,fiber)))
           (_ (error "invalid instruction-body call" form)))))))

  ;;;

  (define-instruction set-global!
    (index fd) -> ((cont))
    (lambda (fiber i v)
      (set-global! fiber i v)))

  ;; (define-instruction record
  ;;   fd* --> ((cont Any))
  ;;   (lambda (fiber as)
  ;;     (stack-push! (fiber-stack fiber) as)))

  (define-instruction iadd
    (fd fd) -> ((cont Int) (throw OverflowException))
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (fx+ a b))))

  (define-instruction isub
    (fd fd) -> ((cont Int) (throw OverflowException))
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (fx- a b))))

  (define-instruction imul
    (fd fd) -> ((cont Int) (throw OverflowException))
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (fx* a b))))

  (define-instruction idiv
    (fd fd) -> ((cont Int) (throw OverflowException) (throw DivideByZeroException))
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (fx/ a b)))))

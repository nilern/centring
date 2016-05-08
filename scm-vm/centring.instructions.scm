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
    args
    code)
  
  (define instructions (make-hash-table))

  (define (valid-intrinsic? op args)
    (and (instr-name? op)
         (valid-arity? op (length args))))

  (define (instr-name? sym)
    (hash-table-exists? instructions sym))

  (define (valid-arity? sym n)
    (= n (length (instruction-args (hash-table-ref instructions sym)))))

  (define-syntax define-instruction
    (ir-macro-transformer
      (lambda (form _ _)
        (match-let (((_ (name . types) (__ (fiber . args) . body)) form))
          `(begin
             (define (,name ,fiber)
               (instruction-body (,fiber ,@args) ,types ,@body))
             (hash-table-set! instructions (quote ,name)
                              (make-instruction (quote ,types) ,name)))))))

  (define-syntax instruction-body
    (ir-macro-transformer
      (lambda (form _ compare?)
        (define (fd? id) (compare? id '<fd>))
        (define (index? id) (compare? id '<i>))
        (match form
          ((_ (fiber arg . args) ((and (? fd?) type) . types) . body)
           `(let ((,arg (fetch-arg! ,fiber)))
              (instruction-body (,fiber ,@args) ,types ,@body)))
          ((_ (fiber arg . args) ((and (? index?) type) . types) . body)
           `(let ((,arg (fetch-instr! ,fiber)))
              (instruction-body (,fiber ,@args) ,types ,@body)))
          ((_ (fiber) '() . body)
           `(begin
              ,@body
              (execute-1! ,fiber)))))))

  (define-instruction (iadd <fd> <fd>)
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (+ a b))))

  (define-instruction (isub <fd> <fd>)
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (- a b))))

  (define-instruction (imul <fd> <fd>)
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (* a b))))

  (define-instruction (idiv <fd> <fd>)
    (lambda (fiber a b)
      (stack-push! (fiber-stack fiber) (quotient a b))))

  (define-instruction (set-global! <i> <fd>)
    (lambda (fiber i v)
      (set-global! fiber i v))))

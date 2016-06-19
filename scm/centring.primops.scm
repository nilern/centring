(module centring.primops
  *
        
  (import scheme chicken)

  (use (srfi 1)
       (srfi 69)
       matchable
       (only miscmacros define-syntax-rule)
       (only clojurian-syntax ->))

  ;;;;

  (define-record-type Instr
    (Instr args conts pure? impl)
    Instr?
    (args Instr-args)
    (conts Instr-conts)
    (pure? Instr-pure?)
    (impl Instr-impl))

  (define (elidable? op)
    (let ((instr (hash-table-ref primops op)))
      (and (Instr-pure? instr)
           (not (any (o (cute eq? 'throw <>) car)
                     (Instr-conts instr))))))

  (define (statement? op)
    (let ((instr (hash-table-ref primops op)))
      (not (or (Instr-pure? instr)
               (any (lambda (cont)
                      (and (eq? (car cont) 'cont) (not (null? (cdr cont)))))
                    (Instr-conts instr))))))

  ;;;;

  (define primops (make-hash-table))

  (define-syntax define-primop
    (syntax-rules (-> -->)
      ((_ name itp args -> conts body ...)
       (begin
         (-primop-impl name itp args body ...)
         (-primop-reg name args conts #f)))
      ((_ name itp args --> conts body ...)
       (begin
         (-primop-impl name itp args body ...)
         (-primop-reg name args conts #t)))))

  (define-syntax -primop-impl
    (syntax-rules ()
      ((_ name itp (args ...) body ...)
       (define (name itp argv)
         (match-let ((#(args ...) argv))
           body ...)))
      ((_ name itp argv body ...)
       (define (name itp argv)
         body ...))))

  (define-syntax-rule (-primop-reg name argpat conts pure?)
    (hash-table-set! primops (quote name)
                     (Instr (quote argpat) (quote conts) pure? name)))

  ;;;;

  (define-primop rec _ argv --> ((cont d))
    argv)

  (define-primop rref _ (rec i) --> ((cont d) (throw Type) (throw Range))
    ;; TODO: better error messages:
    (vector-ref rec (add1 i)))

  (define-primop rset! _ (rec i v) -> ((cont) (throw Type) (throw Range))
    ;; TODO: better error messages:
    (vector-set! rec (add1 i) v))
  
  (define-primop rlen _ (rec) --> ((cont l) (throw Type) (throw Range))
    ;; TODO: better error messages:
    (sub1 (vector-length rec)))

  (define-primop type _ (v) --> ((cont d))
    ;; TODO: types of bits types (3, #f etc.):
    (vector-ref v 0))

  ;;;

  (define-primop bit-eq? _ (a b) --> ((cont d))
    (eq? a b))

  ;;;

  (define-primop iadd _ (a b) --> ((cont d) (throw Overflow))
    ;; TODO: detect overflow
    (fx+ a b))

  (define-primop isub _ (a b) --> ((cont d) (throw Overflow))
    ;; TODO: detect overflow
    (fx- a b))

  (define-primop imul _ (a b) --> ((cont d) (throw Overflow))
    ;; TODO: detect overflow
    (fx* a b))

  (define-primop idiv _ (a b) --> ((cont d) (throw Overflow) (throw ZeroDiv))
    ;; TODO: detect overflow, div by zero
    (fx/ a b)))

  

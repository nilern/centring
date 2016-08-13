(library (ctr primop-impls)
  (export init!)
  (import (rnrs (6))

          (only (ctr primops) define-expression define-statement define-controller)
          (only (ctr env) ns-extend!))

  (define (init!) ; HACK

    ;;;; Globals

    (define-statement (set-global! ns name public? val)
      (ns-extend! ns name public? val))

    ;;;; Arithmetic Operations

    ;;; TODO: detect overflow, div by zero:

    (define-expression (iadd a b)
      (fx+ a b))

    (define-expression (isub a b)
      (fx- a b))

    (define-expression (imul a b)
      (fx* a b))

    (define-expression (idiv a b)
      (fxdiv a b))

    ;;;; Numeric Comparisons

    (define-expression (ieq? a b)
      (fx=? a b))

    (define-expression (ilt? a b)
      (fx<? a b))

    (define-expression (ile? a b)
      (fx<=? a b))

    (define-expression (igt? a b)
      (fx>? a b))

    (define-expression (ige? a b)
      (fx>=? a b))

    ;;;; Equality

    (define-expression (identical? a b)
      (eq? a b))

    ;;;; Conversions

    (define-expression (int->char i)
      (integer->char i))

    (define-expression (char->int c)
      (char->integer c))

    ;;;; Branches

    (define-controller (brf conts c)
      (if c
        (vector-ref conts 0)
        (vector-ref conts 1)))))

  ;; ;;;; Type

  ;; (define-expression (type v)
  ;;   ;; TODO: complete this:
  ;;   (match v
  ;;     (#(t _ ...) t)
  ;;     (($ BytesInstance t _) t)
  ;;     ((? fixnum?) (ns-lookup (ns-ref 'ctr.lang) #f 'Int))
  ;;     ((? flonum?) (ns-lookup (ns-ref 'ctr.lang) #f 'Float))
  ;;     ((? char?) (ns-lookup (ns-ref 'ctr.lang) #f 'Char))
  ;;     ((? FnClosure?) (ns-lookup (ns-ref 'ctr.lang) #f 'Fn))
  ;;     ((? NativeFn?) (ns-lookup (ns-ref 'ctr.lang) #f 'NativeFn))
  ;;     ((? Continuation?) (ns-lookup (ns-ref 'ctr.lang) #f 'Cont))
  ;;     ((? pointer?) (ns-lookup (ns-ref 'ctr.lang) #f 'Ptr))
  ;;     (_ (error "%type not implemented for" v))))

  ;; (define-statement (set-type! r t)
  ;;   (vector-set! r 0 t))

  ;; ;;;; Fn:s

  ;; (define-statement (fn-merge! f1 f2)
  ;;   (dis:fn-merge! f1 f2))

  ;; ;;;; Records

  ;; (define-primop rec
  ;;   (ExprOp (lambda (r) r)))

  ;; (define-expression (shrec r)
  ;;   (vector-copy r 1))

  ;; (define-expression (rcat r1 r2)
  ;;   (let* ((l1 (vector-length r1))
  ;;          (l2 (vector-length r2))
  ;;          (res (make-vector (fx+ l1 (sub1 l2)))))
  ;;     (vector-copy! res 0 r1)
  ;;     (unless (< l2 2)
  ;;       (vector-copy! res l1 r2 1))
  ;;     res))

  ;; (define-expression (rref r i)
  ;;   (vector-ref r (add1 i)))

  ;; (define-statement (rset! r i v)
  ;;   (vector-set! r (add1 i) v))

  ;; (define-expression (rlen r)
  ;;   (sub1 (vector-length r)))

  ;; ;;;; Bytes Types
  
  ;; (define-expression (nbytes type n)
  ;;   (BytesInstance type (make-bytevector n)))     

  ;; (define-expression (bref instance endian signed? size index)
  ;;   (-> instance
  ;;       BytesInstance-bytes
  ;;       ((if signed? bytevector-uint-ref bytevector-sint-ref)
  ;;        index (integer->endianness endian) size)))

  ;; (define-statement (bset! instance endian signed? size index value)
  ;;   (-> instance
  ;;       BytesInstance-bytes
  ;;       ((if signed? bytevector-uint-set! bytevector-sint-set!)
  ;;        index value (integer->endianness endian) size)))

  ;; (define-expression (blen binst)
  ;;   (bytevector-length (BytesInstance-bytes binst)))

  ;; ;;;; Errors

  ;; (define-statement (err kind msg)
  ;;   (abort (make-property-condition 'ctr
  ;;                                   'type (Symbol-name kind)
  ;;                                   'message msg)))

  ;; ;;;; FFI

  ;; (define-statement (ffi-require lib)
  ;;   (lazy-ffi:module (-> lib BytesInstance-bytes utf8->string)))

  ;; (define-expression (ffi-fn ret sym)
  ;;   (let ((name (Symbol-name sym))
  ;;         (ret* (match (vector-ref ret 1)
  ;;                 (($ Symbol 'ctr.lang 'Int) int:)
  ;;                 (($ Symbol 'ctr.lang 'Float) float:)
  ;;                 (($ Symbol 'ctr.lang 'Char) char:)
  ;;                 (($ Symbol 'ctr.lang 'Bool) bool:)
  ;;                 (($ Symbol 'ctr.lang 'Ptr) pointer:)
  ;;                 (name (error "unsupported FFI return type" name)))))
  ;;     (NativeFn name (lazy-ffi:function name (get-uid name)) ret*)))

  ;; (define ffi-symbols (make-hash-table))

  ;; (define get-uid
  ;;   (memoize
  ;;    (lambda (name)
  ;;      (string->symbol (conc "ctr-ffi:"
  ;;                            (current-seconds) (random #x1000000)))))))

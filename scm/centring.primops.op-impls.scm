(module centring.primops.op-impls
  *

  (import scheme chicken)
  (use matchable
       vector-lib
       (only clojurian-syntax doto ->)

       centring.primops
       centring.value
       centring.env
       (prefix centring.dispatch dis:)
       (prefix centring.expand exp:)
       (prefix centring.analyze ana:))

  ;;;; Namespace Operations

  (define-statement (set-ns! ns-name)
    (current-ns (ns-ref (Symbol-name ns-name))))

  (define-controller (require! _ ns-name)
    ;; FIXME: this should be a statement
    (-> (Symbol-name ns-name)
        read-ns
        exp:expand-all
        ana:analyze))

  (define-statement (alias! ns-name as)
    (ns-alias! (current-ns)
               (ns-ref (Symbol-name ns-name))
               (Symbol-name as)))

  (define-statement (rename! var-name as)
    (ns-rename! (current-ns)
                (ns-ref (Symbol-ns var-name))
                (Symbol-name var-name)
                (Symbol-name as)))

  (define-statement (import! ns-name)
    (ns-import! (current-ns) (ns-ref (Symbol-name ns-name))))

  (define-statement (set-global! name val)
    (ns-extend! (current-ns) (Symbol-name name) val))

  ;;;; Type

  (define-expression (type v)
    ;; TODO: complete this:
    (match v
      (#(t _ ...) t)
      ((? FnClosure?) (ns-lookup (ns-ref 'ctr.lang) #f 'Fn))
      ((? Continuation?) (ns-lookup (ns-ref 'ctr.lang) #f 'Cont))
      (_ (error "%type not implemented for" v))))

  (define-statement (set-type! r t)
    (vector-set! r 0 t))

  ;;;; Fn:s

  (define-statement (fn-merge! f1 f2)
    (dis:fn-merge! f1 f2))

  ;;;; Records

  (define-primop rec
    (ExprOp (lambda (r) r)))

  (define-expression (shrec r)
    (vector-copy r 1))

  (define-expression (rcat r1 r2)
    (let ((l1 (vector-length r1))
          (l2 (vector-length r2)))
      (doto (make-vector (fx+ l1 (sub1 l2)))
        (vector-copy! 0 r1)
        (vector-copy! l1 r2 1))))

  (define-expression (rref r i)
    (vector-ref r (add1 i)))

  (define-statement (rset! r i v)
    (vector-set! r (add1 i) v))

  (define-expression (rlen r)
    (sub1 (vector-length r)))

  ;;;; Arithmetic Operations

  ;;; TODO: detect overflow, div by zero:

  (define-expression (iadd a b)
    (fx+ a b))

  (define-expression (isub a b)
    (fx- a b))

  (define-expression (imul a b)
    (fx* a b))

  (define-expression (idiv a b)
    (fx/ a b))

  ;;;; Branches

  (define-controller (brf conts c)
    (if c
      (vector-ref conts 0)
      (vector-ref conts 1)))

  ;;;; Errors

  (define-statement (err kind msg)
    (abort (make-property-condition 'ctr
                                    'type (Symbol-name kind)
                                    'message msg)))

  ;;;; Equality

  (define-expression (identical? a b)
    (eq? a b))

  (define-expression (ieq? a b)
    (fx= a b)))

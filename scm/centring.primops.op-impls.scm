(module centring.primops.op-impls
  *

  (import scheme chicken)
  (use matchable
       (only clojurian-syntax ->)

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
    (match v
      (#(t _ ...) t)
      ((? FnClosure?) (ns-lookup (ns-ref 'ctr.lang) #f 'Fn))
      (_ (error "(type) only implemented for records atm."))))

  (define-statement (set-type! r t)
    (vector-set! r 0 t))

  ;;;; Fn:s

  (define-statement (fn-merge! f1 f2)
    (dis:fn-merge! f1 f2))

  ;;;; Records

  (define-primop rec
    (ExprOp (lambda (r) r)))

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

  ;;;; Equality

  (define-expression (identical? a b)
    (eq? a b))

  (define-expression (ieq? a b)
    (fx= a b)))

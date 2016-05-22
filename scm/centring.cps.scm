(module centring.cps
  *

  (import scheme chicken)
  (use matchable
       (only clojurian-syntax ->)
       coops coops-primitive-objects
       sequences
       vector-lib
       (only data-structures o)

       centring.schring
       (prefix centring.analyze ana:)
       (prefix centring.instructions instr:))

  ;;;;

  (define (branches? op)
    (eq? op 'brf))

  (define (symbol->keyword sym)
    (-> sym symbol->string string->keyword))

  ;;;;

  (define-enum CPS
    (Fn formals types body)
    (Cont formals types body)
    (Fix bindings body)
    (Primop op args conts)

    (Splat val)
    (Global resolution-ns ns name)
    (Clover index)
    (Local name)
    (Const val))

  ;;;; CPS transform

  (define (ast->cps ast)
    (define (cps-k ast k)
      (match ast
        (($ ana:AFn formals types body)
         (let* ((f (gensym 'f))
                (ret (gensym 'r))
                (fn (Fn
                     (vector-append (vector ret)
                                    (smap #() convert-formal formals))
                     (vector-append (vector 'centring.lang/Any)
                                    (smap #() convert-formal types))
                     (cps-k body (lambda (v)
                                   (Primop 'call (vector (Local ret) v) #()))))))
           (Fix (vector (cons f fn)) (k (Local f)))))
        (($ ana:APrimop op args)
         (convert-vector
          args
          (lambda (as)
            (cond
             ((instr:produces-result? op)
              (let ((res (gensym 'v)))
                (Primop op as
                        (vector
                         (Fn (vector res)
                             (vector (instr:result-type op))
                             (k (Local res)))))))
             ((branches? op))
             ((eq? op 'call))
             (else (error "TBD" op))))))
        (($ ana:AGlobal res-ns ns name) (k (Global res-ns ns name)))
        (($ ana:ALocal name) (k (Local name)))
        (($ ana:AConst val) (k (Const val)))
        (_ (error "unable to CPS-convert" ast))))
    
    (define (convert-vector vec k)
      (let* ((len (vector-length vec))
             (res (make-vector len)))
        (define (cpsv i)
          (if (= i len)
            (k res)
            (cps-k (vector-ref vec i)
                   (lambda (v) (vector-set! res i v) (cpsv (add1 i))))))
        (cpsv 0)))
    
    (define (convert-formal formal)
      (match formal
        (($ ana:ASplat f) (Splat f))
        (f f)))
    
    (cps-k ast (lambda (v) (Primop 'halt (vector v) #()))))

  ;;;; Traversal

  (define-generic (fold-cps f node))

  (define-method (fold-cps (f #t) (node <Fn>))
    (f node (fold-cps f (.body node))))

  (define-method (fold-cps (f #t) (node <Fix>))
    (f node
       (fmap (o (cute fold-cps f <>) cdr) (.bindings node))
       (fold-cps f (.body node))))

  (define-method (fold-cps (f #t) (node <Primop>))
    (f node
       (fmap (cute fold-cps f <>) (.args node))
       (fmap (cute fold-cps f <>) (.conts node))))

  (define-method (fold-cps (f #t) (node <Local>))
    (f node))

  (define-method (fold-cps (f #t) (node <Const>))
    (f node))

  ;;;; Printing

  (define-generic (cps->sexpr-rf node))

  (define-method (cps->sexpr-rf (node <Fn>) br)
    (define formal->sexpr
      (match-lambda
        (($ Splat f) `($... ,f))
        (f f)))
    `($fn ,(smap '() formal->sexpr (.formals node))
          ,(smap '() formal->sexpr (.types node))
          ,br))

  (define-method (cps->sexpr-rf (node <Fix>) vrs br)
    `($letrec ,(map list
                    (smap '() car (.bindings node))
                    (vector->list vrs))
              ,br))

  (define-method (cps->sexpr-rf (node <Primop>) ars krs)
    (match krs
      (#()
       `(,(symbol-append '% (.op node)) ,@(vector->list ars)))
      (#(($fn (res) (t) br))
       `($let ((,res ,(symbol->keyword t)
                     (,(symbol-append '% (.op node)) ,@(vector->list ars))))
          ,br))))

  (define-method (cps->sexpr-rf (node <Local>))
    (.name node))

  (define-method (cps->sexpr-rf (node <Const>))
    (.val node))

  (define (cps->sexpr cexp)
    (fold-cps cps->sexpr-rf cexp)))

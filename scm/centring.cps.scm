(module centring.cps
  *

  (import scheme chicken)
  (use matchable
       (only clojurian-syntax ->)
       coops coops-primitive-objects
       sequences
       vector-lib
       (only data-structures o complement)

       centring.schring
       (prefix centring.analyze ana:)
       (prefix centring.instructions instr:))

  ;;;;

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
        ((? ana:AFn?)
         (let ((f (gensym 'f)))
           (cps-k (ana:AFix (vector (cons f ast)) (ana:ALocal f)) k)))
        (($ ana:AFix bindings body)
         (define (convert-fixfun bind)
           (match-let (((f . ($ ana:AFn formals types body)) bind)
                       (ret (gensym 'r)))
             (cons f
                   (Fn
                    (vector-append (vector ret)
                                   (smap #() convert-formal formals))
                    (vector-append (vector 'centring.lang/Any)
                                   (smap #() convert-formal types))
                    (cps-k body (lambda (v)
                                  (Primop 'call (vector (Local ret) v) #())))))))
         (Fix (smap #() convert-fixfun bindings) (cps-k body k)))
        (($ ana:APrimop op args) (convert-primop op args k))
        
        (($ ana:ASplat val) (cps-k val (lambda (v) (k (Splat v)))))
        (($ ana:AGlobal res-ns ns name) (k (Global res-ns ns name)))
        (($ ana:ALocal name) (k (Local name)))
        (($ ana:AConst val) (k (Const val)))
        (_ (error "unable to CPS-convert" ast))))

    (define (convert-primop op args k)
      (cond
       ((instr:produces-result? op)
        (convert-args
         args
         (lambda (as)
           (let ((res (gensym 'v)))
             (Primop op as
                     (vector
                      (Fn (vector res)
                          (vector (instr:result-type op))
                          (k (Local res)))))))))
       ((instr:bin-branch? op)
        (receive (args conts) (split (complement ana:AFn?) args)
          (convert-args
           args
           (lambda (as)
             (let* ((ret (gensym 'r))
                    (res (gensym 'v))
                    (jump (lambda (v) (Primop 'call (vector (Local ret) v) #())))
                    (convert-cont
                     (lambda (cont)
                       (Fn (smap #() convert-formal (ana:AFn-formals cont))
                           (smap #() convert-formal (ana:AFn-types cont))
                           (cps-k (ana:AFn-body cont) jump)))))
               (Fix (vector (cons ret (Fn (vector res) (vector 'centring.lang/Any)
                                          (k (Local res)))))
                    (Primop op as (smap #() convert-cont conts))))))))
       ((instr:statement? op)
        (convert-args
         args
         (lambda (as)
           (Primop op as
                   (vector
                    (Fn #() #()
                        (cps-k
                         (ana:APrimop 'record (vector (ana:AGlobal 'centring.lang
                                                                   'centring.lang
                                                                   'Tuple)))
                         k)))))))
       ((eq? op 'call)
        (convert-args
         args
         (lambda (as)
           (let* ((ret (gensym 'r))
                  (res (gensym 'v))
                  (jump (lambda (v) (Primop 'call (vector (Local ret) v) #()))))
             (Fix (vector (cons ret
                                (Fn (vector res) (vector 'centring.lang/Any)
                                    (k (Local res)))))
                  (Primop 'call (vector-append (subvector as 0 1)
                                               (vector (Local ret))
                                               (subvector as 1)) #()))))))
       (else (error "unable to CPS-convert primop" ast))))
    
    (define (convert-args vec k)
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

  (define-method (fold-cps (f #t) (node <Cont>))
    (f node (fold-cps f (.body node))))

  (define-method (fold-cps (f #t) (node <Fix>))
    (f node
       (fmap (o (cute fold-cps f <>) cdr) (.bindings node))
       (fold-cps f (.body node))))

  (define-method (fold-cps (f #t) (node <Primop>))
    (f node
       (fmap (cute fold-cps f <>) (.args node))
       (fmap (cute fold-cps f <>) (.conts node))))

  (define-method (fold-cps (f #t) (node <Splat>))
    (f node (fold-cps f (.val node))))

  (define-method (fold-cps (f #t) (node <Global>))
    (f node))

  (define-method (fold-cps (f #t) (node <Clover>))
    (f node))

  (define-method (fold-cps (f #t) (node <Local>))
    (f node))

  (define-method (fold-cps (f #t) (node <Const>))
    (f node))

  ;;;

  (define-method (fmap (f #t) (node <Fn>))
    (Fn (.formals node) (.types node) (f (.body node))))

  (define-method (fmap (f #t) (node <Cont>))
    (Cont (.formals node) (.types node) (f (.body node))))

  (define-method (fmap (f #t) (node <Fix>))
    (Fix (smap #() (lambda (b) (cons (car b) (f (cdr b)))) (.bindings node))
         (f (.body node))))

  (define-method (fmap (f #t) (node <Primop>))
    (Primop (.op node) (smap #() f (.args node)) (smap #() f (.conts node))))

  (define-method (fmap (f #t) (node <Splat>))
    (Splat (f (.val node))))

  (define-method (fmap (f #t) (node <Global>))
    node)

  (define-method (fmap (f #t) (node <Clover>))
    node)

  (define-method (fmap (f #t) (node <Local>))
    node)

  (define-method (fmap (f #t) (node <Const>))
    node)

  ;;;; Printing

  (define-generic (cps->sexpr-rf node))

  (define-method (cps->sexpr-rf (node <Fn>) br)
    `($fn ,(smap '() formal->sexpr (.formals node))
          ,(smap '() formal->sexpr (.types node))
          ,br))

  (define-method (cps->sexpr-rf (node <Cont>) br)
    `($k ,(smap '() formal->sexpr (.formals node))
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
      (#(('$fn (res) (t) br))
       `($flet ((,res ,(symbol->keyword t)
                      (,(symbol-append '% (.op node)) ,@(vector->list ars))))
               ,br))
      (#(('$k (res) (t) br))
       `($let ((,res ,(symbol->keyword t)
                     (,(symbol-append '% (.op node)) ,@(vector->list ars))))
              ,br))
      (_
       `(,(symbol-append '% (.op node)) ,@(vector->list ars)
         ,@(vector->list krs)))))

  (define-method (cps->sexpr-rf (node <Splat>) ar)
    `($... ,ar))

  (define-method (cps->sexpr-rf (node <Global>))
    (symbol-append (or (.ns node) '@@)
                   ;'<= (.resolution-ns node)
                   ana:ns-sep (.name node)))

  (define-method (cps->sexpr-rf (node <Clover>))
    `(@ ,(.index node)))

  (define-method (cps->sexpr-rf (node <Local>))
    (.name node))

  (define-method (cps->sexpr-rf (node <Const>))
    (.val node))

  (define (cps->sexpr cexp)
    (fold-cps cps->sexpr-rf cexp))
  
  (define formal->sexpr
    (match-lambda
     (($ Splat f) `($... ,f))
     (f f)))

  ;;;; Contification
                 
  ;;; ATM this is trivial

  (define (contify cexp)
    (define (cfy node)
      (match node
        (($ Primop op args conts)
         ;; since `conts` are anonymous fns they can't have any other callsites:
         (Primop op args (smap #()
                               (match-lambda
                                (($ Fn formals types body)
                                 (Cont formals types body)))
                               conts)))
        (_ node)))
    (prewalk cfy cexp)))
                 

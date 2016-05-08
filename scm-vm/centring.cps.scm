(module centring.cps
  *

  (import scheme chicken)
  (use typed-records
       (only matchable match)
       sequences
       vector-lib

       (only centring.util mapl)
       (prefix centring.analyze ana:))

  (define-record Block
    (label : symbol)
    formals ; ::vector<symbol U Splat<symbol>>
    types   ; ::vector<symbol U Splat<symbol>>
    body)   ; ::CPS
  (define-record Fix
    (defns : (vector-of (struct Block)))
    body) ; ::CPS
  (define-record If
    cond   ; ::fetch-descr
    tcont  ; ::CPS
    fcont) ; ::CPS
  (define-record Primop
    (op : symbol)
    args  ; ::vector<fetch-descr U Splat<fetch-descr>>
    (res : (vector-of symbol))
    cont) ; ::CPS
  (define-record App
    callee ; ::fetch-descr
    args)  ; ::vector<fetch-descr U Splat<fetch-descr>>

  (define-record Splat
    fd) ; ::fetch-descr

  ;; fetch-descr
  (define-record Local
    (name : symbol))
  (define-record Clover
    (index : fixnum))
  (define-record Global
    (ns : symbol)
    (name : symbol))
  (define-record Label
    (name : symbol))
  (define-record Const
    val) ; ::dvalue

  ;;;; CPS conversion

  (define (cps-c ast c)
    (match ast
      (($ ana:Block label formals types body)
       (let ((ret (gensym 'r)))
         (make-Block
          label
          (vector-append (vector ret)
                         (smap #() convert-formal formals))
          (vector-append (vector 'centring.lang/Any)
                         (smap #() convert-formal types))
          (cps-c body (lambda (v) (make-App (make-Local ret) (vector v)))))))
      (($ ana:Fix defns body)
       (make-Fix (smap #() (cute cps-c <> #f) defns) (cps-c body c)))
      (($ ana:If cond then else)
       (cps-c cond
              (lambda (cv)
                (let* ((ret (gensym 'r))
                       (res (gensym 'v))
                       (jump (lambda (v)
                               (make-App (make-Local ret) (vector v)))))
                  (make-Fix
                   (vector
                    (make-Block ret (vector res) (vector 'centring.lang/Any)
                                (c (make-Local res))))
                   (make-If cv (cps-c then jump) (cps-c else jump)))))))
      (($ ana:Primop op args)
       (cps-vector args
                   (lambda (as)
                     (if (produces-result? op)
                       (let ((res (gensym 'v)))
                         (make-Primop op as (vector res) (c (make-Local res))))
                       (make-Primop op as #()
                                    (cps-c (ana:make-Primop 'record
                                            (vector (ana:make-Global
                                                     'centring.lang 'Tuple)))
                                           c))))))
      (($ ana:App callee args)
       (cps-c callee
              (lambda (f)
                (let* ((ret (gensym 'r))
                       (res (gensym 'v))
                       (jump (lambda (as)
                               (make-App f (vector-append (vector ret) as)))))
                  (make-Fix
                   (vector
                    (make-Block ret (vector res) (vector 'centring.lang/Any)
                                (c (make-Local res))))
                   (cps-vector args jump))))))
      (($ ana:Splat val)
       (cps-c val (lambda (v) (c (make-Splat v)))))
      (($ ana:Local name) (c (make-Local name)))
      (($ ana:Global ns name) (c (make-Global ns name)))
      (($ ana:Label name) (c (make-Label name)))
      (($ ana:Const name) (c (make-Const name)))
      (_ (error "unable to cps-convert" ast))))
                  
  (define (cps-vector vec c)
    (let* ((len (vector-length vec))
           (res (make-vector len)))
      (define (cpsv i)
        (if (= i len)
          (c res)
          (cps-c (vector-ref vec i)
                 (lambda (v) (vector-set! res i v) (cpsv (add1 i))))))
      (cpsv 0)))
  
  (define (convert-formal formal)
    (match formal
      (($ ana:Splat f) (make-Splat f))
      (f f)))

  (define (produces-result? op)
    (not (eq? op 'set-global!)))

  ;;;; Convert to S-expr (for debugging)

  (define (cps->sexp cexp)
    (match cexp
      (($ Block label formals types body)
       `((,label ,(mapl cps->sexp formals) ,(mapl cps->sexp types)
                 ,(cps->sexp body))))
      (($ Fix defns body)
       `($letfn ,(mapl cps->sexp defns) ,(cps->sexp body)))
      (($ If cond then else)
       `($if ,(cps->sexp cond) ,(cps->sexp then) ,(cps->sexp else)))
      (($ Primop op args #(res) cont)
       `($let ((,res (,(symbol-append '% op) ,@(mapl cps->sexp args))))
              ,(cps->sexp cont)))
      (($ Primop 'halt #(arg) #() #f)
       `(%halt ,(cps->sexp arg)))
      (($ Primop op args #() cont)
       `($let ((,(symbol-append '% op) ,@(mapl cps->sexp args)))
              ,(cps->sexp cont)))
      (($ App callee args)
       `(,(cps->sexp callee) ,@(mapl cps->sexp args)))

      (($ Splat val)
       `($... ,(cps->sexp val)))

      (($ Local name) name)
      (($ Clover i) `(@ ,i))
      (($ Global #f name) (symbol-append '@@/ name))
      (($ Global ns name) (symbol-append ns '/ name))
      (($ Label name) name)
      (($ Const val) val)
      (_ cexp))))

(module centring.cps
  *

  (import scheme chicken)
  (use typed-records
       (only matchable match match-let match-lambda)
       (only miscmacros define-syntax-rule)
       (only clojurian-syntax ->)
       (only anaphora aif)
       (srfi 69)
       sequences
       vector-lib

       (only centring.util mapl)
       (only centring.instructions side-effecting?)
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
    (results : (vector-of symbol))
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
                               (make-App f (vector-append
                                            (vector (make-Label ret)) as)))))
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

  ;;;; Traversal

  (define (prefold f cexp)
    (match cexp
      (($ Block _ _ _ body)
       ((f 'Block) cexp (prefold f body)))
      (($ Fix defns body)
       ((f 'Fix) cexp (smap #() (cute prefold f <>) defns) (prefold f body)))
      (($ If cond then else)
       ((f 'If) cexp (prefold f cond) (prefold f then) (prefold f else)))
      (($ Primop _ args _ cont)
       ((f 'Primop) cexp (smap #() (cute prefold f <>) args)
        (if cont (prefold f cont) cont)))
      (($ App callee args)
       ((f 'App) cexp (prefold f callee) (smap #() (cute prefold f <>) args)))

      (($ Splat arg)
       ((f 'Splat) cexp (prefold f arg)))

      ((? Local?)  ((f 'Local) cexp))
      ((? Clover?) ((f 'Clover) cexp))
      ((? Global?) ((f 'Global) cexp))
      ((? Label?)  ((f 'Label) cexp))
      ((? Const?)  ((f 'Const) cexp))
      (_ (error "not a CPS expression" cexp))))

  (define-syntax-rule
    (define-prefolder name ((tag (node subresults ...)) body ...) ...)
    (define (name tg)
      (case tg
        ((tag) (lambda (node subresults ...) body ...)) ...
        (else (error "unknown prefold dispatch tag" tg)))))

  ;;;; Convert to S-expr (for debugging)

  (define (cps->sexp cexp)
    (define-prefolder cs-r
      ((Block (node br))
       (match-let ((($ Block label formals types _) node))
             `(,label ,(mapl formal->sexp formals) ,(mapl formal->sexp types)
                      ,br)))
      ((Fix (node drs br))  `($letfn ,(vector->list drs) ,br))
      ((If (node cr tr er)) `($if ,cr ,tr ,er))
      ((Primop (node ars cr))
         (cond
          ((eq? (Primop-op node) 'halt)
           `(%halt ,(vector-ref ars 0)))
          ((zero? (vector-length (Primop-results node)))
           `($let ((,(symbol-append '% (Primop-op node)) ,(vector->list ars)))
                  ,cr))
          (else
           `($let ((,(vector-ref (Primop-results node) 0)
                    (,(symbol-append '% (Primop-op node)) ,(vector->list ars))))
                  ,cr))))
      ((App (_ cr ars)) `(,cr ,(vector->list ars)))
      
      ((Splat (_ ar)) `($... ,ar))
      
      ((Local (node))  (Local-name node))
      ((Clover (node)) `(@ ,(Clover-index node)))
      ((Global (node)) (symbol-append (or (Global-ns node) '@@) '/ (Global-name node)))
      ((Label (node))  (Label-name node))
      ((Const (node))  (Const-val node)))
    (prefold cs-r cexp))

  (define (formal->sexp formal)
    (match formal
      (($ Splat name) `($... ,name))
      (_ formal))))

  ;; ;;;; Collect Contraction Information

  ;; (define (contraction-db cexp)
  ;;   (let ((db (make-hash-table)))
  ;;     (prewalk (cute collect-db! db <>) cexp)
  ;;     db))

  ;; (define (collect-db! db node)
  ;;   (match node
  ;;     (($ Block label formals types _)
  ;;      (hash-table-set! db label (alist->hash-table `((type . (fn ,types))
  ;;                                                     (usecount . 0))))
  ;;      (vector-for-each
  ;;       (lambda (f t)
  ;;         (hash-table-set! db f (alist->hash-table `((type . ,t)
  ;;                                                    (usecount . 0)))))
  ;;       formals types))
  ;;     (($ Primop _ _ #(res) _)
  ;;      (hash-table-set! db res (alist->hash-table `((usecount . 0)))))
  ;;     ((or ($ Local name) ($ Label name))
  ;;      (hash-table-update! (hash-table-ref db name) 'usecount add1)))
  ;;   node)

  ;; ;;;; Eta Contraction

  ;; (define (mark-etable! db label replacement)
  ;;   (hash-table-set! (hash-table-ref db label) 'replacement replacement))

  ;; (define (replace-etable! db name)
  ;;   (let ((facts (hash-table-ref db name)))
  ;;     (aif (hash-table-ref/default facts 'replacement #f)
  ;;       (begin
  ;;         (hash-table-update! facts 'usecount sub1)
  ;;         it)
  ;;       name)))

  ;; ;;;; Useless-variable Elimination

  ;; (define (eliminate-useless db cexp)
  ;;   (prewalk (cute eliminate-useless db <>) cexp))

  ;; (define (eliminate-useless-f db node)
  ;;   (match node
  ;;     (($ Fix defns body)
  ;;      (make-Fix
  ;;       (filter #() (lambda (defn) (not (not-used? db (Block-label defn)))) defns)
  ;;       body))
  ;;     (($ Primop op args #(res) cont)
  ;;      (if (and (not-used? db op) (not (side-effecting? op)))
  ;;        cont
  ;;        node))
  ;;     (_ node)))

  ;; (define (not-used? db name)
  ;;   (zero? (usecount db name))))
      

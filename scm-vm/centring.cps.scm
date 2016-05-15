(module centring.cps
  *

  (import scheme chicken)
  (use typed-records
       (only matchable match match-let match-lambda match-lambda*)
       (only miscmacros define-syntax-rule)
       (only clojurian-syntax ->)
       (only anaphora aif)
       (only data-structures o complement identity)
       (only extras printf) ; just for debugging
       (srfi 69)
       persistent-hash-map
       sequences
       vector-lib

       (only centring.util mapl map-merge-with zipmap subtype?)
       (prefix centring.instructions instr:)
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
                               (make-App (make-Label ret) (vector v)))))
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

  (define (fmap f node)
    (match node
      (($ Block label formals types body)
       (make-Block label formals types (f body)))
      (($ Fix defns body)
       (make-Fix (smap #() f defns) (f body)))
      (($ If cond then else)
       (make-If (f cond) (f then) (f else)))
      (($ Primop op args results cont)
       (make-Primop op (smap #() f args) results (if cont (f cont) cont)))
      (($ App callee args)
       (make-App (f callee) (smap #() f args)))
      (($ Splat arg)
       (make-Splat (f arg)))
      ((or (? Local?) (? Global?) (? Clover?) (? Label?) (? Const?))
       node)))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

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
           `($let ((,(symbol-append '% (Primop-op node)) ,@(vector->list ars)))
                  ,cr))
          (else
           `($let ((,(vector-ref (Primop-results node) 0)
                    (,(symbol-append '% (Primop-op node)) ,@(vector->list ars))))
                  ,cr))))
      ((App (_ cr ars)) `(,cr ,@(vector->list ars)))
      
      ((Splat (_ ar)) `($... ,ar))
      
      ((Local (node))  (Local-name node))
      ((Clover (node)) `(@ ,(Clover-index node)))
      ((Global (node)) (symbol-append (or (Global-ns node) '@@)
                                      '/ (Global-name node)))
      ((Label (node))  (Label-name node))
      ((Const (node))  (Const-val node)))
    (prefold cs-r cexp))

  (define (formal->sexp formal)
    (match formal
      (($ Splat name) `($... ,name))
      (_ formal)))

  ;;;; Utility Passes

  (define (replace-local rps node)
    (match node
      (($ Local name) (map-ref rps name node))
      (_ node)))

  (define (replace-label rps node)
    (match node
      (($ Label name) (hash-table-ref/default rps name node))
      (_ node)))

  ;;;; Collect Contraction Information

  (define-record LocalInfo
    usecount
    type)
  (define-record LabelInfo
    usecount
    ftypes
    formals
    body)

  (define-record-printer (LocalInfo li out)
    (fprintf out "#<LocalInfo ~S ~S>"
             (LocalInfo-usecount li) (LocalInfo-type li)))
  (define-record-printer (LabelInfo li out)
    (fprintf out "#<LabelInfo ~S ~S ~S>"
             (LabelInfo-usecount li) (LabelInfo-ftypes li)
             (LabelInfo-formals li)))

  (define (usecount db name)
    (match (map-ref db name)
      (($ LocalInfo usecount _) usecount)
      (($ LabelInfo usecount _) usecount)))
  
  (define (useless? db name)
    (zero? (usecount db name)))

  (define (type db ref)
    (match ref
      (($ Local name) (LocalInfo-type (map-ref db name)))
      (($ Label name) (LabelInfo-ftypes (map-ref db name)))
      (_ #f)))

  (define (ftypes db name)
    (LabelInfo-ftypes (map-ref db name)))

  (define (formals db name)
    (LabelInfo-formals (map-ref db name)))

  (define (body db name)
    (LabelInfo-body (map-ref db name)))

  (define (collect-db cexp)
    (define combine-entries
      (match-lambda*
       ((($ LocalInfo uc1 'centring.lang/Any) ($ LocalInfo uc2 t2))
        (make-LocalInfo (+ uc1 uc2) t2))
       ((($ LocalInfo uc1 t1) ($ LocalInfo uc2 'centring.lang/Any))
        (make-LocalInfo (+ uc1 uc2) t1))
       ((($ LabelInfo uc1 #(($ Splat 'centring.lang/Any)) _ _)
         ($ LabelInfo uc2 t2 formals body))
        (make-LabelInfo (+ uc1 uc2) t2 formals body ))
       ((($ LabelInfo uc1 t1 formals body) ($ LabelInfo uc2))
        (make-LabelInfo (+ uc1 uc2) t1 formals body))
       ((a b) (error "can't combine" a b))))
    
    (define merge (cute map-merge-with combine-entries <> <>))

    (define (collect-formals formals types)
      (define r
        (match-lambda*
         ((_ res ($ Splat f) ($ Splat t))
          (cons (cons f (make-LocalInfo 0 'centring.lang/Tuple)) res))
         ((_ res f t)
          (cons (cons f (make-LocalInfo 0 t)) res))))
      (alist->map (reverse (vector-fold r '() formals types))))
    
    (define-prefolder db-collector
      ((Block (node bdb))
       (match-let ((($ Block label formals types body) node))
         (-> bdb
             (merge (persistent-map label
                                    (make-LabelInfo 0 types formals body)))
             (merge (collect-formals formals types)))))
      ((Fix (_ drs br))
       (foldl merge br drs))
      ((If (_ cr tr er))
       (-> cr (merge tr) (merge er)))
      ((Primop (node ars cr))
       (match node
         (($ Primop op _ #(res) _)
          (merge (foldl merge (or cr (persistent-map)) ars)
                 (persistent-map res (make-LocalInfo 0 (instr:produced-type op)))))
         (($ Primop op _ #() _)
          (foldl merge (or cr (persistent-map)) ars))))
      ((App (_ cr ars))
       (foldl merge cr ars))

      ((Splat (_ ar)) ar)

      ((Local (node))
       (persistent-map (Local-name node)
                       (make-LocalInfo 1 'centring.lang/Any)))
      ((Clover (_)) (persistent-map))
      ((Global (_)) (persistent-map))
      ((Label (node))
       (persistent-map (Label-name node)
                       (make-LabelInfo 1 (vector
                                          (make-Splat 'centring.lang/Any))
                                       #f #f)))
      ((Const (_)) (persistent-map)))
    
      (prefold db-collector cexp))

  ;;;; Eta Contraction
  
  (define (eta-contract db cexp)
    (letrec ((eta-rps (make-hash-table))
             (localize-formal
              (match-lambda
               (($ Splat f) (make-Splat (make-Local f)))
               (f (make-Local f))))
             (is-any?
              (match-lambda
               ((or ($ Splat 'centring.lang/Any) 'centring.lang/Any) #t)
               (_ #f)))
             (update-rps!
              (match-lambda
               (($ Block label fps types ($ App f args))
                (when (and (equal? (smap #() localize-formal fps) args)
                           (vector-every is-any? types))
                  (hash-table-set! eta-rps label (replace-label eta-rps f))))
               ((? Block?))))
             (contract
              (match-lambda
               ((and ($ Fix defns _) node)
                (for update-rps! defns)
                node)
               (node (replace-label eta-rps node)))))
      ;; need prewalk so that eta-rps is updated before replacements are tried:
      (prewalk contract cexp)))

  ;;;; Beta-contraction

  ;; need to run `eliminate-useless` after this:
  (define (beta-contract db cexp)
    (define (contract node)
      (match node
        (($ App ($ Label callee) args)
         (if (and (= (usecount db callee) 1)
                  (vector-every (lambda (a ft) (subtype? ft (type db a)))
                                args (ftypes db callee)))
           (postwalk (cute replace-local (zipmap (formals db callee) args) <>)
                     (body db callee))
           node))
        (_ node)))
    ;; using prewalk is potentially more aggressive:
    (prewalk contract cexp))

  ;;;; Useless-variable Elimination

  (define (eliminate-useless db cexp)
    (define (elim node)
      (match node
        (($ Fix defns body)
         (let ((defns* (filter #()
                               (complement (o (cute useless? db <>) Block-label))
                               defns)))
           (if (zero? (vector-length defns*))
             body
             (make-Fix defns* body))))
        (($ Primop op args #(res) cont)
         (if (and (useless? db res) (instr:elidable? op))
           cont
           node))
        (($ Primop op args #() cont) node)

        ((or (? Block?) (? If?) (? App?)
             (? Splat?)
             (? Local?) (? Global?) (? Clover?) (? Label?) (? Const?))
         node)))
    ;; need postwalk here so that the bodies and conts don't slip through:
    (postwalk elim cexp)))

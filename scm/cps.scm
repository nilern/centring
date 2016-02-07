(use matchable
     (only extras pretty-print)
     (only (srfi 13) string-prefix? string-suffix?))

(define (primitive? ast)
  (if (symbol? ast)
    (let ((s (symbol->string ast)))
      (and (string-prefix? "__" s) (string-suffix? "__" s)))
    #f))

(define (atomic? ast)
  (match ast
    ((or ('fn (? list?) _)
         (? symbol?)
         (? number?)
         (? string?)
         (? boolean?)
         'Nil)
     #t)
     (else #f)))

(define (cps-atomic ast)
  (match ast
    (('fn (and (? list?) vars) body)
      (let (($k (gensym '$k)))
        `(fn (,@vars ,$k) 
           ,(cps body $k))))

    ('call-cc '(fn (f cc) (f (fn (x _) (cc x)) cc)))

    ((or (? symbol?)
         (? number?)
         (? string?)
         (? boolean?)
         'Nil)
     ast)

    (else (error "Not an atomic expr!"))))

(define (cps-f ast kf)
  (match ast
    ((? atomic?)
     (kf (cps-atomic ast)))
    (('if cast tast east)
      ; We have to reify the cont to avoid
      ; a possible code blow-up:
     (let* (($rv (gensym '$rv))
            (k `(fn (,$rv) ,(kf $rv))))
       (cps-f cast (lambda (aast)
                     `(if ,aast 
                        ,(cps tast k)
                        ,(cps east k))))))
    (('set! var expr)
     (cps-f expr (lambda (aast) `(set-then! ,var ,aast ,(kf 'Nil)))))
    ; WARNING: This transformation is not hygienic 
    ; if the continuation k-ast references any of the 
    ; bound variables!  
    ; 
    ; Use this transformation only on alphatized 
    ; programs!
    (('letrec vas ,body)
     `(letrec (,@(map (lambda (va) (list (var va) (cps-atomic (cadr va))))
                      vas))
        ,(cps-f body kf)))
    ((_ _ . ...)
     (let* (($rv (gensym '$rv))
            (k `(fn (,$rv) ,(kf $rv))))
       (cps ast k)))))

(define (cps-inject asts kf)
  (match asts
    ('() (kf '()))
    ((ast . rasts) (cps-f ast 
                          (lambda (hd)
                            (cps-inject rasts
                                        (lambda (tl) (kf (cons hd tl)))))))))

(define (cps ast k-ast)
  (match ast
    ((? atomic?)
     `(,k-ast ,(cps-atomic ast)))
    (('do expr)
     (cps expr k-ast))
    (('do expr . exprs)
     (cps-f expr (lambda (_) (cps `(do ,@exprs) k-ast))))
    (('if cast tast east)
      ; We have to bind the cont to avoid
      ; a possible code blow-up:
      (let (($k (gensym '$k)))
        `((fn (,$k)
            ,(cps-f cast (lambda (aast)
                           `(if ,aast 
                              ,(cps tast $k)
                              ,(cps east $k)))))
          ,k-ast)))
    (('set! var expr)
     (cps-f expr (lambda (aast) `(set-then! ,var ,aast (,k-ast (void))))))
    ; WARNING: This transformation is not hygienic 
    ; if the continuation k-ast references any of the 
    ; bound variables!  
    ; 
    ; Use this transformation only on alphatized 
    ; programs!
    (('letrec vas ,body)
     `(letrec (,@(map (lambda (va) (list (var va) (cps-atomic (cadr va))))
                      vas))
        ,(cps body k-ast)))
    (((and p (? primitive?)) . es)
     (cps-inject es (lambda ($es) `((cps ,p) ,@$es ,k-ast))))
    ((f . es)
     (cps-f f (lambda ($f)
                (cps-inject es (lambda ($es) `(,$f ,@$es ,k-ast))))))))

;;;

(define (main)
  (pretty-print (cps '((fn (< * dec)
                         (if (< n 1)
                           1
                           (* n (fact (dec n)))))
                       (fn (a b) (__lt_i64__ a b))
                       (fn (a b) (__mul_i64__ a b))
                       (fn (n) (__sub_i64__ n 1)))
                     'k)))

(main)

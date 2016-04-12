(module centring.expand
  *

  (import scheme chicken)
  (use (only matchable match))
  
  (define (ctr-expand-1 sexp)
    (match sexp
      (('if cond then else) `(centring.sf/if ,cond ,then ,else))
      (('do . stmts) `(centring.sf/do ,@stmts))     
           
      (_ sexp)))     

  (define (ctr-expand expr)
    (let ((expansion (ctr-expand-1 expr)))
      (if (eq? expansion expr)
        expr
        (ctr-expand expansion))))

  ;; Does not handle macro shadowing (i.e. (let ((if (fn (n) ...))) (if ...)))
  (define (ctr-expand-all expr)
    (let ((expansion (ctr-expand expr)))
      (if (list? expansion)
        (map ctr-expand-all (ctr-expand expr))
        expansion))))

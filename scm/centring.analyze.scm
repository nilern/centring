(module centring.analyze
  *
  
  (import scheme chicken)
  (use matchable
       r6rs.bytevectors

       centring.util
       centring.value
       centring.ast
       centring.env
       (only centring.primops op-purpose)
       (only centring.dispatch dnf inject-dnf))

  (define (analyze sexp)
    (match sexp
     ((? special-form?) (analyze-sf sexp))
      
     ((? intrinsic?) (analyze-intr sexp))
     
     ((? symbol?) (call-with-values (lambda () (ns-name sexp)) Symbol))

     ((? literal?) (Const sexp))

     ((? string?)
      (Const (BytesInstance
              (ns-lookup (ns-ref 'ctr.lang) #f 'String)
              (string->utf8 sexp))))

     ((callee . args)
      (Primop 'apply
              (vector (analyze callee)
                      (Primop 'rec
                              (mapv analyze (cons 'ctr.lang/Tuple args))
                              #f))
              #f))

     (_ (error "unable to analyze" sexp))))
  
  (define (analyze-sf sexp)
    (match (cons (name (car sexp)) (cdr sexp))
      (('fn arg . cases)
       (Fn arg (mapv (match-lambda
                      ((cond body)
                       (cons (inject-dnf (dnf (analyze cond)))
                             (analyze body))))
                     cases) #f))

      (('letrec bindings body)
       (Fix (mapv
             (match-lambda ((var val) (cons var (analyze val))))
             bindings)
            (analyze body)))
      
      (('do . stmts)
       (Do (mapv analyze stmts)))
      
      (('quote (and (? symbol?) sym))
       (Const (analyze sym)))
      (('quote v)
       (Const v))

      (_ (error "invalid special form" sexp))))

  (define (analyze-intr sexp)
    (case (op-purpose (name (car sexp)))
      ((ctrl)
       (Primop (name (car sexp))
               (vector (analyze (cadr sexp)))
               (mapv analyze (cddr sexp))))
      (else
       (Primop (name (car sexp))
               (mapv analyze (cdr sexp))
               #f))))

  (define (special-form? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'ctr.sf)))

  (define (intrinsic? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'ctr.intr))))

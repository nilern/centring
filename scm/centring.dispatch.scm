(module centring.dispatch
  *

  (import scheme chicken)
  (use matchable
       (srfi 1)
       vector-lib
       sequences
       (srfi 42)
       data-structures
       (only miscmacros until inc!)
       dyn-vector

       centring.util
       centring.value
       centring.ast)

  ;;;; DNF conversion

  ;; DNF-convert a Fn case condition:
  (define (dnf ast)
    (define (wrap node)
      (Primop 'bior (vector (Primop 'band (vector node) #f)) #f))
    
    (define (combine-dnfs-with f default subnodes)
      (match subnodes
        (#() default)
        (#(node) node)
        ((? vector?)
         (Primop 'bior
                 (foldl f (Primop-args (peek subnodes)) (pop subnodes))
                 #f))))
    
    (match ast
      (($ Primop 'bior args _)
       ;; convert args and flatten the resulting `or` of `or`:s:
       (combine-dnfs-with (lambda (acc v) (vector-append acc (Primop-args v)))
                          (wrap (Const #f)) (mapv dnf args)))
      
      (($ Primop 'band args _)
       ;; convert args and distribute `and` over them:
       (combine-dnfs-with
        (lambda (acc v)
          (vector-ec (:vector l acc) (:vector r (Primop-args v))
            (Primop 'band
                    (vector-append (Primop-args l) (Primop-args r))
                    #f)))
        (wrap (Const #t)) (mapv dnf args)))
      
      (($ Primop 'bnot #(arg) _)
       ;; Use some Boolean algebra laws and reconvert:
       (match arg
         (($ Primop 'bior args _) ; De Morgan
          (dnf (Primop 'band
                       (mapv (lambda (v)
                               (Primop 'bnot (vector v) #f))
                             args)
                       #f)))
         (($ Primop 'band args _) ; De Morgan
          (dnf (Primop 'bior
                       (mapv (lambda (v)
                               (Primop 'bnot (vector v) #f))
                             args)
                       #f)))
         (($ Primop 'bnot #(arg) _) ; double negation
          (dnf arg))
         (_ (wrap (Primop 'bnot (vector (dnf-convert arg)) #f)))))
      
      (_ (wrap (dnf-convert ast)))))

  (define (inject-dnf ast)
    (match ast
      (($ Primop 'bior args _)
       (mapv inject-dnf args))
      (($ Primop 'band args _)
       (mapv inject-dnf args))
      (_ ast)))

  ;; Traverse an AST and DNF-convert Fn case conditions:
  (define (dnf-convert ast)
    (match ast
      (($ Fn arg cases #f)
       (Fn arg
           (mapv (match-lambda
                  ((cond . body)
                   (cons (inject-dnf (dnf cond)) (dnf-convert body))))
                 cases)
           #f))
      (_ (node-map dnf-convert ast))))

  ;;;;

  (define (fn-merge! cl1 cl2)
    (match-let* ((($ FnClosure formal1 _ _ caseq1) cl1)
                 (($ FnClosure formal2 _ cases2 caseq2) cl2)
                 (caseq2* (make-queue)))
      ;; OPTIMIZE: make all args have the same name at analysis-time
      (define (replace-formal v)
        (match v
          (($ Symbol #f (? (cute eq? formal1 <>))) (Symbol #f formal2))
          (_ v)))
      (define (replace-case-formal case)
        (match-let ((#(cond body env) case))
          (vector (postwalk replace-formal cond)
                  (postwalk replace-formal body)
                  env)))
      ;; cases already in use at cl2:
      (dynvector-for-each
       (lambda (_ case)
         (queue-add! caseq1 (replace-case-formal case)))
       cases2)
      ;; pending cases of cl2:
      (until (queue-empty? caseq2)
        (let ((case (queue-remove! caseq2)))
          (queue-add! caseq1 (replace-case-formal case))
          (queue-add! caseq2* case)))
      ;; undo the damage done to cl2:
      (set! (FnClosure-caseq cl2) caseq2*)))

  ;;;;

  ;;; OPTIMIZE: prevent (Const (or #t #f)) from getting to the dag-construction
  ;;; OPTIMIZE: emit (case (type ...) ...) instead of (brf ...)

  ;;; a `df` is a dynvector<#(#(AST ...) AST env)>

  (define (build-lookup-dag df)
    ;; OPTIMIZE: memoization
    (define (build-sub-dag/assuming expr truthy? cs es)
      (let* ((cs* (target-cases expr truthy? cs))
             (es* (target-expr-cls expr cs* es)))
        (build-sub-dag cs* es*)))
    (define (build-sub-dag cs es)
      (if (dynvector-empty? es)
        (compute-target cs)
        (let ((cond-cl (pick-expr-cl es cs)))
          (Primop 'brf
                  (vector cond-cl)
                  (vector
                   (build-sub-dag/assuming (Closure-expr cond-cl) #t cs es)
                   (build-sub-dag/assuming (Closure-expr cond-cl) #f cs es))))))
    (build-sub-dag df (df-expr-cls df)))

  (define (compute-target cs)
    ;; TODO: deal with overrides ("min<=_method")
    ;; TODO: emit actual throwing code
    (case (dynvector-length cs)
      ((0) (Const (Symbol 'Exception 'NoMethod)))
      ((1) (let ((case (dynvector-ref cs 0)))
             (Closure (vector-ref case 1) (vector-ref case 2))))
      (else (Const (Symbol 'Exception 'AmbiguousMethod)))))

  (define (target-cases expr truthy? cs)
    (define (atom-passes? atom)
      (if truthy?
        (match atom
          (($ Primop 'bnot #((? (cute equal? expr <>))) _) #f)
          (_ #t))
        (match atom
          ((? (cute equal? expr <>)) #f)
          (_ #t))))
    (define (case-passes? case)
      (all? atom-passes? (vector-ref case 0)))
    (dynvector-filter case-passes? cs))

  (define (target-expr-cls expr cs* es)
    (dvset-intersection
     (dynvector-remove (o (cute equal? expr <>) Closure-expr) es)
     (df-expr-cls cs*)))

  (define (pick-expr-cl es cs)
    ;; OPTIMIZE: implement heuristics
    (dynvector-ref es 0))

  (define (df-inject! cl)
    (match-let ((($ FnClosure _ _ cases caseq) cl))
      (until (queue-empty? caseq)
        (dynvector-push! cases (queue-remove! caseq)))
      cases))

  (define (df-expr-cls df)
    (define atom-expr
      (match-lambda
        (($ Primop 'bnot #(expr) _) expr)
        (expr expr)))
    (let ((res (make-dynvector 0 #f)))
      (dynvector-for-each
       (lambda (_ case)
         (doseq (atom (vector-ref case 0))
           (dynvector-push! res (Closure (atom-expr atom)
                                         (vector-ref case 2)))))
       df)
      res)))

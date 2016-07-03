(module centring.dispatch
  *

  (import scheme chicken)
  (use matchable
       (srfi 1)
       vector-lib
       sequences
       (srfi 42)

       centring.util
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

  (define (tautology? cond)
    ;; TODO: don't make lists just for `any` and `every`:
    (match cond
      (($ Primop 'bior #(args ...) _)
       (any tautology? args))
      (($ Primop 'band #(args ...) _)
       (every tautology? args))
      (($ Primop 'bnot #(($ Primop yield #(($ Const #f)) _)) _)
       #t)
      (($ Primop 'yield #((? tautology?)) _)
       #t)
      (($ Const #t)
       #t)
      (_ #f))))

  ;;;;

  ;; a `df` is a data structure that matches
  ;; #((#((or ($ Primop 'band #(expr) _) expr) ...) . body-name) ...)

  ;; (define (df-inject cases)
  ;;   (define (handle-case case)
  ;;     (match-let (((clauses . body) case)
  ;;                 (body-name (gensym 'm)))
  ;;       (values (mapv (cute cons <> body-name) clauses) body-name body)))
  ;;   (mappendv (lambda (case)
  ;;               (receive (cases body-name body) (handle-case case)
  ;;                 (push! (cons body-name (Fn #() #(#t 

  ;; (define (build-lookup-dag df)
  ;;   (let* ((cases (dfc-inject df))
  ;;          (exprs (dfc-exprs cases)))
  ;;     (build-sub-dag cases exprs)))

  ;; (define (build-sub-dag cases exprs)
  ;;   (if (null? exprs)
  ;;     (Primop 'apply
  ;;             (Local (compute-target cases))
  ;;             (Primop 'rec
  ;;                     (vector (Global 'centring.lang 'centring.lang 'Tuple))
  ;;                     #f)
  ;;             #f)
  ;;     (let ((expr (pick-expr exprs cases)))
  ;;       (Primop 'brf (vector expr)
  ;;               (mapv (lambda (truthy?)
  ;;                       (call-with-values
  ;;                           (lambda () (dfc-filter cases exprs expr truthy?))
  ;;                         build-sub-dag))
  ;;                     '(#t #f))))))

  ;; (define (pick-expr exprs cases)
  ;;   (car exprs))

  ;; (define (compute-target cases)
  ;;   (match cases
  ;;     (((_ . f)) f)
  ;;     ('() 'no-method)
  ;;     (_ 'ambiguous-methods)))

  ;; (define (dfc-inject cases)
  ;;   (map '() vector->list cases))

  ;; (define (dfc-filter cases exprs expr truthy?)
  ;;   (let* ((cases* (filter '() (cute case-passes? expr truthy? <>) cases))
  ;;          (exprs* (filter (cute equal? <> expr) (dfc-exprs cases))))
  ;;     (values cases* exprs*)))

  ;; (define (case-passes? expr truthy? case)
  ;;   (every (cute atom-passes? expr truthy? <>) (car case)))

  ;; (define (atom-passes? expr truthy? atom)
  ;;   (match atom
  ;;     (($ Primop 'bnot #((? (cute equal? <> expr) _)))
  ;;      (not truthy?))
  ;;     ((? (cute equal? <> expr))
  ;;      truthy?)
  ;;     (_ #t)))

  ;; (define (dfc-exprs cases)
  ;;   (append-map case-exprs cases))

  ;; (define (case-exprs case)
  ;;   (map (match-lambda (($ Primop 'bnot #(expr) _) expr) (expr expr))
  ;;        (car case))))
       

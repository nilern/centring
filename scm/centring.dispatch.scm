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

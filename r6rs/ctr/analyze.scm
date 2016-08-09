(library (ctr analyze)
  (export analyze)
  (import (rnrs (6))

          (only (util) partial)
          (only (util collections) mapv)
          
          (only (ctr util) ctr-error literal? ns-name ns name)
          (ctr ast)
          (only (ctr primops) op-purpose))

  ;;;; Analyze

  (define (analyze sexp)
    (cond
     ((special-form? sexp)
      (analyze-sf sexp))
     ((intrinsic? sexp)
      (analyze-intr sexp))
     ((literal? sexp)
      (make-Const sexp))
     ((symbol? sexp)
      (call-with-values (lambda () (ns-name sexp)) (partial make-Global #f)))
     (else
      (ctr-error "unable to analyze" sexp))))

  ;;;; Special Forms

  (define (special-form? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'ctr.sf)))
  
  (define (analyze-sf sexp)
    (case (name (car sexp))
      ;; (('fn arg . cases)
      ;;  (Fn arg (mapv (match-lambda
      ;;                 ((cond body)
      ;;                  (cons (inject-dnf (dnf (analyze cond)))
      ;;                        (analyze body))))
      ;;                cases) #f))
      ((letrec)
       (make-Fix (mapv (lambda (binding)
                         (cons (car binding) (analyze (cadr binding))))
                       (cadr sexp))
                 (analyze (caddr sexp))))
      ((do)
       (make-Do (mapv analyze (cdr sexp))))
      ((quote) ; TODO: symbols
       (make-Const (cadr sexp)))
      (else
       (ctr-error "invalid special form" sexp))))

  ;;;; Intrinsics

  (define (intrinsic? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'ctr.intr)))

  (define (analyze-intr sexp)
    (let ((opname (name (car sexp))))
      (case (op-purpose opname)
        ((ctrl)
         (make-Primop opname
                      (vector (analyze (cadr sexp)))
                      (mapv analyze (cddr sexp))))
        (else
         (make-Primop opname
                      (mapv analyze (cdr sexp))
                      #f))))))

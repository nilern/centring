(library (ctr analyze)
  (export analyze)
  (import (rnrs (6))

          (only (util) partial)
          (only (util collections) mapv)
          
          (only (ctr util) ctr-error literal? ns-name ns name)
          (ctr ast)
          (prefix (ctr dispatch) dnf:)
          (only (ctr primops) op-purpose))

  ;; TODO: embed primops in AST
  ;; TODO: identifier specialization & ns-resolution pass

  ;;;; Analyze

  (define (analyze sexp)
    (cond
     ((special-form? sexp)
      (analyze-sf sexp))
     ((intrinsic? sexp)
      (analyze-intr sexp))
     ((symbol? sexp)
      (call-with-values (lambda () (ns-name sexp)) (partial make-Global #f)))
     ((pair? sexp)
      (make-Primop 'apply
                   (vector (analyze (car sexp))
                           (make-Primop 'rec
                                        (mapv analyze
                                              (cons 'ctr.lang/Tuple (cdr sexp)))
                                        #f))
                   #f))
     ((literal? sexp)
      (make-Const sexp))
     (else
      (ctr-error "unable to analyze" sexp))))

  ;;;; Special Forms

  (define (special-form? sexp)
    (and (pair? sexp)
         (symbol? (car sexp))
         (eq? (ns (car sexp)) 'ctr.sf)))
  
  (define (analyze-sf sexp)
    (case (name (car sexp))
      ((fn)
       (make-Fn (cadr sexp)
                (mapv (lambda (case)
                        (cons (dnf:inject (dnf:dnf (analyze (car case))))
                              (analyze (cadr case))))
                      (cddr sexp))))
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

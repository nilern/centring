(library (ctr analyze)
  (export analyze resolve!)
  (import (rnrs (6))

          (only (util) partial -> ->>)
          (only (util collections) mapv mapl)
          (util dynvector)
          
          (only (ctr util) ctr-error literal? ns-name ns name)
          (ctr ast)
          (ctr env)
          (prefix (ctr cek) cek:)
          (prefix (ctr expand) exp:)
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
                      #f)))))

  ;;;; Name Resolution

  (define (resolve! ast)
    (->> ast
         flattened-stmts
         (dynvector-filter exec!)
         dynvector->vector
         make-Do))
  
  (define exec!
    ;; TODO: error handling
    (let ((import-set #f))
      (lambda (stmt)
        (primop-case stmt
          ((set-ns!)
           (current-ns (-> stmt Primop-args (vector-ref 0) Const-val
                           ns-ref))
           #f)
          ((require!)
           (let ((ns-name (-> stmt Primop-args (vector-ref 0) Const-val))
                 (curr-ns (current-ns)))
             ;; FIXME: DRY (also appears in ctri.scm):
             (-> ns-name
                 read-ns
                 exp:expand-all
                 analyze
                 resolve!
                 cek:interpret)
             (current-ns curr-ns)
             #f))
          ((alias!)
           (let* ((args (Primop-args stmt))
                  (other (-> args (vector-ref 0) Const-val))
                  (as (-> args (vector-ref 1) Const-val)))
             (add-alias! (current-ns) (ns-ref other) as)
             #f))
          ((start-import!)
           (set! import-set (-> stmt Primop-args (vector-ref 0) Const-val
                                ns-ref
                                ns-publics))
           #f)
          ((end-import!)
           (set! import-set #f)
           #f)
          ((only!)
           (let ((oids (->> stmt Primop-args (mapl Const-val))))
             (set! import-set (dynvector-filter
                               (lambda (id-var) (member (car id-var) oids))
                               import-set))
             #f))
          ((except!)
           (let ((oids (->> stmt Primop-args (mapl Const-val))))
             (set! import-set (dynvector-filter
                               (lambda (id-var) (not (member (car id-var) oids)))
                               import-set))
             #f))
          (else stmt)))))

  (define (flattened-stmts ast)
    (define stmt-trs (make-dynvector))
    (define (flatten! node)
      (if (Do? node)
        (vector-for-each flatten! (Do-stmts node))
        (dynvector-push! stmt-trs node)))
    (flatten! ast)
    stmt-trs))

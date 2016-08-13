(library (ctr analyze)
  (export analyze resolve!)
  (import (rnrs (6))
          (only (chezscheme) vector-copy)

          (only (util) partial identity if-let -> ->>)
          (only (util collections) mapv mapl)
          (util dynvector)
          
          (only (ctr util) ctr-error literal? ns-name ns name)
          (ctr ast)
          (ctr env)
          (prefix (ctr cek) cek:)
          (prefix (ctr expand) exp:)
          (prefix (ctr dispatch) dnf:)
          (only (ctr primops) get-op op-purpose))

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
      ((quote)
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
         ;; OPTIMIZE: use transducer
         (dynvector-filter exec!)
         (dynvector-map (resolve '()))
         dynvector->vector
         make-Do))

  (define (resolve env)
    (define (resf ast)
      (cond
       ((Fn? ast)
        ;; add arg into env and recur:
        (node-map (resolve (cons (Fn-arg ast) env)) ast))
       ((Primop? ast)
        ;; recur and embed primop object into node:
        (let* ((op-name (Primop-op ast))
               (impl (get-op op-name))
               (args* (vector-map resf (Primop-args ast)))
               (conts* (if-let (conts (Primop-conts ast))
                         (vector-map resf conts)
                         #f))
               (ast* (make-Primop impl args* conts*)))
          (when (eqv? op-name 'set-global!)
            ;; embed ns into node as well:
            (vector-set! args* 0 (make-Const (current-ns))))
          ast*))
       ((Global? ast)
        ;; specialize locals, embed ns into remaining globals:
        (let ((ns-name (Global-ns ast))
              (name (Global-name ast)))
          (or (and (not ns-name)
                   (memq name env)
                   (make-Local name))
              (make-Global (current-ns) ns-name name))))
       (else
        (node-map resf ast))))
    resf)
        
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
           ;; FIXME: DRY (almost the same above and below)
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

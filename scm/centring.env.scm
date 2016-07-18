(module centring.env
  *

  (import scheme chicken)
  (use matchable
       (srfi 69)
       persistent-hash-map
       (only clojurian-syntax ->)
       (only anaphora aif)
       (only extras read-file)
       (only files make-pathname)
       (only irregex irregex-split)

       (only centring.util try))

  ;;;; Vars
  
  (define-record Var
    name
    val)

  (define (var-ref var)
    (Var-val var))

  (define (var-set! var v)
    (Var-val-set! var v))

  ;;;; Namespaces

  (define-record Ns
    name
    mappings
    refers
    aliases)

  (define ns-registry (make-hash-table))

  ;;; FIXME: can't use #f as default as it is a valid Centring value

  ;; Fetch the ns if it exists, else create it:
  (define (ns-ref name)
    (try
      (hash-table-ref ns-registry name)
      (catch _
        (let ((ns (make-Ns name
                           (make-hash-table)
                           (make-hash-table)
                           (make-hash-table))))
          (hash-table-set! ns-registry name ns)
          ns))))
  
  (define current-ns (make-parameter (ns-ref 'ctr.user)))

  ;; Fetch the var:
  (define (ns-resolve ns ns-name name)
    (if ns-name
      (try
        (-> (or (hash-table-ref/default (Ns-aliases ns) ns-name #f)
                (hash-table-ref ns-registry ns-name))
            Ns-mappings
            (hash-table-ref name))
        (catch _
          (error "unbound variable" ns-name name)))
      (try
        (hash-table-ref (Ns-mappings ns) name)
        (catch _
          (try
            (hash-table-ref (Ns-refers ns) name)
            (catch _
              (error "unbound variable" ns-name name)))))))

  ;; Add an alias to ns:
  (define (ns-alias! ns other as)
    (hash-table-set! (Ns-aliases ns) as other))

  ;; Add a renaming to ns:
  (define (ns-rename! into from name as)
    (try
      (let ((var (hash-table-ref (Ns-mappings from) name)))
        (hash-table-set! (Ns-refers into) as var))
      (catch _
        (error "cannot refer" (Ns-name from) name))))

  (define (ns-import! into from)
    (hash-table-for-each (Ns-mappings from)
     (cute hash-table-set! (Ns-refers into) <> <>)))

  ;; Fetch the value in a var:
  (define (ns-lookup ns ns-name name)
    (var-ref (ns-resolve ns ns-name name)))

  ;; Reset the var if it exists in the mappings of ns, else create it:
  (define (ns-extend! ns name v)
    (try
      (var-set! (hash-table-ref (Ns-mappings ns) name) v)
      (catch _
        (let ((var (make-Var (symbol-append (Ns-name ns) name) v)))
          (hash-table-set! (Ns-mappings ns) name var)))))

  (define ctr-path (make-parameter '()))
    
  (define (read-ns ns-name)
    (let recur ((path (ctr-path)))
      (if (pair? path)
        (let* ((ns-components (irregex-split #\. (symbol->string ns-name)))
               (filename
                (make-pathname
                 (car path) (foldl make-pathname "" ns-components) ".ctr")))
          (if (file-exists? filename)
            `(do ,@(read-file filename))
            (recur (cdr path))))
        (error "unable to locate ns with path" ns-name (ctr-path)))))

  ;;;; Env

  (define make-env persistent-map)

  (define (env-lookup env ns-name name)
    (if ns-name
      (ns-lookup (current-ns) ns-name name)
      (let ((env-res (map-ref env name '())))
        (if (null? env-res)
          (ns-lookup (current-ns) ns-name name)
          env-res))))

  (define env-extend map-add)

  (define env-merge map-merge))

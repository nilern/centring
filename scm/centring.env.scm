(module centring.env
  *

  (import scheme chicken)
  (use matchable
       (srfi 69)
       persistent-hash-map
       (only clojurian-syntax ->)
       (only anaphora aif))

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

  ;; Fetch the ns if it exists, else create it:
  (define (ns-ref name)
    (or (hash-table-ref/default ns-registry name #f)
        (let ((ns (make-Ns name
                           (make-hash-table)
                           (make-hash-table)
                           (make-hash-table))))
          (hash-table-set! ns-registry name ns)
          ns)))

  ;; Fetch the var:
  (define (ns-resolve ns ns-name name)
    (if ns-name
      (or (-> (or (hash-table-ref/default (Ns-aliases ns) ns-name #f)
                  (hash-table-ref ns-registry ns-name))
              Ns-mappings
              (hash-table-ref/default name #f))
          (error "unbound variable" ns-name name))
      (or (hash-table-ref/default (Ns-mappings ns) name #f)
          (hash-table-ref/default (Ns-refers ns) name #f)
          (error "unbound variable" ns-name name))))

  ;; Add an alias to ns:
  (define (ns-alias! ns other as)
    (hash-table-set! (Ns-aliases ns) as other))

  ;; Add a renaming to ns:
  (define (ns-rename! into from name as)
    (aif (hash-table-ref/default (Ns-mappings from) name #f)
      (hash-table-set! (Ns-refers into) as it)
      (error "cannot refer" (Ns-name from) name)))

  ;; Fetch the value in a var:
  (define (ns-lookup ns ns-name name)
    (var-ref (ns-resolve ns ns-name name)))

  ;; Reset the var if it exists in the mappings of ns, else create it:
  (define (ns-extend! ns name v)
    (aif (hash-table-ref/default (Ns-mappings ns) name #f)
      (var-set! it v)
      (let ((var (make-Var (symbol-append (Ns-name ns) name) v)))
        (hash-table-set! (Ns-mappings ns) name var))))

  ;;;; Env

  (define-record-type Env
    (Env mappings ns)
    Env?
    (mappings Env-mappings)
    (ns Env-ns))

  (define (make-env ns)
    (Env (persistent-map) ns))

  (define (env-lookup env ns-name name)
    (or (and (not ns-name)
             (map-ref (Env-mappings env) name))
        (ns-lookup (Env-ns env) ns-name name)))

  (define (env-extend env name val)
    (Env (map-add (Env-mappings env) name val) (Env-ns env))))

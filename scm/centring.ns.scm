(module centring.ns
  *

  (import scheme chicken)
  (use (srfi 69)
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

  ;; Fetch the ns if it exists, else create it:
  (define (ns-ref registry name)
    (or (hash-table-ref/default registry name #f)
        (let ((ns (make-Ns name
                           (make-hash-table)
                           (make-hash-table)
                           (make-hash-table))))
          (hash-table-set! registry name ns)
          ns)))

  ;; Fetch the var:
  (define (resolve registry ns ns-name name)
    (if ns-name
      (-> (or (hash-table-ref/default (Ns-aliases ns) ns-name #f)
              (hash-table-ref registry ns-name))
          Ns-mappings
          (hash-table-ref name))
      (or (hash-table-ref/default (Ns-mappings ns) name #f)
          (hash-table-ref (Ns-refers ns) name))))

  ;; Fetch the value in a var:
  (define (lookup registry ns ns-name name)
    (var-ref (resolve registry ns ns-name name)))

  ;; Reset the var if it exists in the mappings of ns, else create it:
  (define (extend! ns name v)
    (aif (hash-table-ref/default (Ns-mappings ns) name #f)
      (var-set! it v)
      (let ((var (make-Var (symbol-append (Ns-name ns) name) v)))
        (hash-table-set! (Ns-mappings ns) name var)))))
    
      
      
    

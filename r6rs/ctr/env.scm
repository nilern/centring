(library (ctr env)
  (export ns-ref current-ns
          ns-lookup ns-extend!)
  (import (rnrs (6))

          (only (util) let-cc if-let when-let symbol-append)

          (only (ctr util) ns-sep ctr-error))

  ;;;; Var

  (define-record-type Var
    (fields
     (immutable name)
     (mutable val var-ref var-set!)
     (mutable public?)))

  ;;;; Ns

  (define-record-type Ns
    (fields
     (immutable name)
     (immutable mappings)
     (immutable refers)
     (immutable aliases)))

  ;;;;

  (define ns-registry (make-eq-hashtable))

  (define (ns-ref name)
    (or (hashtable-ref ns-registry name #f)
        (let ((ns (make-Ns name
                           (make-eq-hashtable)
                           (make-eq-hashtable)
                           (make-eq-hashtable))))
          (hashtable-set! ns-registry name ns)
          ns)))

  (define current-ns (ns-ref 'ctr.user))

  ;;;;

  (define (ns-lookup ns ns-name name)
    ;; TODO: raise properly, untangle if-let:s
    ;; FIXME: honour `Var-public?`
    (var-ref
     (let-cc return
       (if ns-name
         (begin
           (when-let (vns (or (hashtable-ref (Ns-aliases ns) ns-name #f)
                              (hashtable-ref ns-registry ns-name #f)))
             (let ((res (hashtable-ref (Ns-mappings vns) name #f)))
               (unless (null? res)
                 (return res))))
           (ctr-error "unbound variable" ns-name name))
         (or (hashtable-ref (Ns-mappings ns) name #f)
             (hashtable-ref (Ns-refers ns) name #f)
             (ctr-error "unbound variable" ns-name name))))))

  (define (ns-extend! ns name public? v)
    (if-let (var (hashtable-ref (Ns-mappings ns) name #f))
      (var-set! var v)
      (let ((var (make-Var (symbol-append (Ns-name ns) ns-sep name) v public?)))
        (hashtable-set! (Ns-mappings ns) name var)))))

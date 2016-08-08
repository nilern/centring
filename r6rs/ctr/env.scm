(library (ctr env)
  (export ns-ref current-ns
          ns-extend! add-alias!
          make-env env-assoc
          lookup)
  (import (chezscheme) ; TODO: how to use r6rs instead (parameters!)?

          (only (util) if-let defrecord symbol-append)

          (only (ctr util) ns-sep ctr-error))

  ;;;; Var

  (define-record-type Var
    (fields
     (immutable name)
     (mutable val var-ref var-set!)
     (mutable public?)))

  ;;;; Ns

  (defrecord (Ns name mappings refers aliases))

  ;;;; Namespace Registry

  (define ns-registry (make-eq-hashtable))

  (define (ns-ref name)
    (or (hashtable-ref ns-registry name #f)
        (let ((ns (make-Ns name
                           (make-eq-hashtable)
                           (make-eq-hashtable)
                           (make-eq-hashtable))))
          (hashtable-set! ns-registry name ns)
          ns)))

  (define current-ns (make-parameter (ns-ref 'ctr.user)))

  ;;;; Namespace Operations

  ;;; TODO: these need clarification

  (define (ns-lookup ns ns-name name)
    (if ns-name
      (if-let (vns (or (hashtable-ref (Ns-aliases ns) ns-name #f)
                       (hashtable-ref ns-registry ns-name #f)))
        (if-let (var (hashtable-ref (Ns-mappings vns) name #f))
          (if (or (Var-public? var) (eq? vns ns))
            (var-ref var)
            (ctr-error "variable is private" ns-name name))
          (ctr-error "unbound variable" ns-name name))
        (ctr-error "unbound namespace" ns-name))
      (var-ref (or (hashtable-ref (Ns-mappings ns) name #f)
                   (hashtable-ref (Ns-refers ns) name #f)
                   (ctr-error "unbound variable" ns-name name)))))

  (define (ns-extend! ns name public? v)
    ;; FIXME: honour `Var-public?`
    (if-let (var (hashtable-ref (Ns-mappings ns) name #f))
      (var-set! var v)
      (let ((var (make-Var (symbol-append (Ns-name ns) ns-sep name) v public?)))
        (hashtable-set! (Ns-mappings ns) name var))))

  (define (add-alias! ns other as)
    (hashtable-set! (Ns-aliases ns) as other))

  ;;;; Local Environments

  (define (make-env) '())

  (define (env-ref env name default)
    (if-let (kv (assq name env))
      (cdr kv)
      default))

  (define (env-assoc env name val)
    (cons (cons name val) env))

  ;;;; General Variable Lookup

  (define (lookup env ns-name name)
    (if ns-name
      (ns-lookup (current-ns) ns-name name)
      (let ((env-res (env-ref env name '())))
        (if (null? env-res)
          (ns-lookup (current-ns) ns-name name)
          env-res)))))
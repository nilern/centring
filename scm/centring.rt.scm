(module centring.rt
  *

  (import scheme chicken)
  (use (only extras read-file)
       (only files make-pathname)
       irregex
       (srfi 69)

       centring.ns)

  ;;;;

  (define-record-type Fiber
    (Fiber ns-reg curr-ns locals)
    Fiber?
    (ns-reg Fiber-ns-reg)
    (curr-ns Fiber-curr-ns)
    (locals Fiber-locals))

  (define (make-Fiber)
    (let ((ns-reg (make-hash-table)))
      (extend! (ns-ref ns-reg 'centring.lang) 'Tuple #f) ; HACK
      (Fiber ns-reg (ns-ref ns-reg 'centring.user) (make-hash-table))))

  (define (Fiber-local-ref fiber local)
    (hash-table-ref (Fiber-locals fiber) local))

  (define (Fiber-local-set! fiber local v)
    (hash-table-set! (Fiber-locals fiber) local v))

  (define (Fiber-global-ref fiber res-ns ns-name name)
    (let* ((ns-reg (Fiber-ns-reg fiber))
           (ns (hash-table-ref ns-reg res-ns)))
      (lookup ns-reg ns ns-name name)))

  (define (Fiber-global-set! fiber name v)
    (extend! (Fiber-curr-ns fiber) name v))

  ;;;;

  (define (read-ns full-path ns-name)
    (let recur ((path full-path))
      (if (pair? path)
        (let* ((ns-components (irregex-split #\. (symbol->string ns-name)))
               (filename
                (make-pathname
                 (car path) (foldl make-pathname "" ns-components) ".ctr")))
          (if (file-exists? filename)
            `(do ,@(read-file filename))
            (recur (cdr path))))
        (error "unable to locate ns with path" ns-name full-path)))))

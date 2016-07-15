(module centring.util
  *

  (import scheme chicken)
  (use (only (srfi 13) string-index)
       (srfi 69)
       vector-lib
       sequences
       dyn-vector
       (only data-structures complement)
       (only miscmacros define-syntax-rule let/cc))

  (define-syntax defrecord
    (er-macro-transformer
     (lambda (sexp r _)
       (let ((name (caadr sexp))
             (fields (cdadr sexp)))
         `(,(r 'define-record-type) ,name
           (,name ,@fields)
           ,(symbol-append name '?)
           ,@(map (lambda (field)
                    (list field (symbol-append name '- field)))
                  fields))))))

  (define (ns-name sym)
    (let* ((symstr (symbol->string sym))
           (i (string-index symstr #\/)))
      (if (or (not i) (= i 0) (= i (sub1 (string-length symstr))))
        (values #f sym)
        (values (string->symbol (substring symstr 0 i))
                (string->symbol (substring symstr (add1 i)))))))

  (define (name sym)
    (receive (_ name) (ns-name sym) name))

  (define (ns sym)
    (receive (ns _) (ns-name sym) ns))

  (define ns-sep '/)

  (define (literal? v)
    (or (fixnum? v) (boolean? v)))

  (define mapv (cute smap #() <> <>))

  (define (map-pair f p)
    (cons (f (car p)) (f (cdr p))))

  (define (hash-table-zip ks vs)
    (let ((res (make-hash-table)))
      (vector-for-each
       (lambda (_ k v)
         (hash-table-set! res k v))
       ks vs)
      res))

  (define dynvector-empty? (o zero? dynvector-length))

  (define (dynvector-push! dvec v)
    (dynvector-set! dvec (dynvector-length dvec) v))

  (define (dynvector-member? dvec val)
    (let/cc return
      (dynvector-for-each
       (lambda (_ v)
         (when (equal? v val)
           (return #t)))
       dvec)
      #f))

  (define (dynvector-filter pred? dvec)
    (let ((res (make-dynvector 0 #f)))
      (dynvector-fold
       (lambda (_ _ v)
         (when (pred? v) (dynvector-push! res v)))
       #f dvec)
      res))

  (define (dynvector-remove pred? dvec)
    (dynvector-filter (complement pred?) dvec))

  (define (dvset-intersection dvset1 dvset2)
    (let ((res (dynvector-copy dvset1)))
      (dynvector-for-each
       (lambda (_ v)
         (when (dynvector-member? res v)
           (dynvector-push! res v)))
       dvset2)
      res))

  (define-syntax try
    (syntax-rules (catch)
      ((try body ... (catch exn handling ...))
       (handle-exceptions exn
         (begin handling ...)
         body ...))))

  (define-syntax doseq
    (syntax-rules ()
      ((doseq ((v i) coll) body ...)
       (for* (lambda (coll it) (let ((i (index it)) (v (elt coll it))) body ...)) coll))
      ((doseq (v coll) body ...)
       (for (lambda (v) body ...) coll)))))

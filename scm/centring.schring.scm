(module centring.schring
  *

  (import scheme chicken)
  (use coops coops-primitive-objects
       sequences
       (only vector-lib vector-fold vector-map))

  ;;;

  (define-generic (fmap f v))

  (define-method (fmap (f #t) (ls <list>))
    (map f ls))

  (define-method (fmap (f #t) (vec <vector>))
    (vector-map (lambda (_ v) (f v)) vec))

  (define-generic (fold f v coll))

  (define-method (fold (f #t) (v #t) (ls <list>))
    (foldl f v ls))

  (define-method (fold (f #t) (v #t) (vec <vector>))
    (vector-fold (lambda (_ acc v) (f acc v)) v vec))

  ;;;

  (define-syntax define-enum
    (er-macro-transformer
     (lambda (form r _)
       (define (classify recname)
         (symbol-append '< recname '>))
       
       (define (predicativize recname)
         (symbol-append recname '?))
       
       (define (rec-accessorize recname slotname)
         (symbol-append recname '- slotname))
       
       (define (accessor recname classname slotname)
         `(,(r 'define-method) (,(symbol-append '|.| slotname)
                                (,(r 'rec) ,classname))
           (,(rec-accessorize recname slotname) ,(r 'rec))))
       
       (let* ((abstract (cadr form))
              (variants (cddr form))
              (abstract-class (classify abstract)))
         `(,(r 'begin)
            (,(r 'define-class) ,abstract-class) ; abstract supertype
            ,@(map                               ; variants/subtypes
               (lambda (variant)
                 (let* ((recname (car variant))
                        (classname (classify recname))
                        (slotnames (cdr variant)))
                   `(,(r 'begin)
                     ;; The record type (for constructor, pattern matching)
                     (,(r 'define-record-type) ,recname
                      (,recname ,@slotnames) ,(predicativize recname)
                      ,@(map (lambda (slotname)
                               `(,slotname ,(rec-accessorize recname slotname)))
                             slotnames))
                     ;; The corresponding class (for dispatch):
                     (,(r 'define-primitive-class) ,classname (,abstract-class)
                      ,(predicativize recname))
                     ;; Dot-functions (for nicer field access):
                     ,@(map (lambda (slotname)
                              (accessor recname classname slotname))
                            slotnames))))
               variants))))))

  (define-syntax doseq
    (syntax-rules ()
      ((_ (item items) body ...) (for (lambda (item) body ...) items))))

  ;;;;

  (define-enum Option
    (Some val)
    (None))

  (define-generic (unwrap container))

  (define-method (unwrap (s <Some>))
    (.val s))

  (define-method (unwrap (_ <None>))
    (error "tried to unwrap a None"))

  (define-generic (unwrap-or container))

  (define-method (unwrap-or (s <Some>) _)
    (.val s))

  (define-method (unwrap-or (_ <None>) default)
    default))


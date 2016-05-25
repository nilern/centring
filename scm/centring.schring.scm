(module centring.schring
  *

  (import scheme chicken)
  (use coops coops-primitive-objects
       sequences
       (only data-structures identity)
       (only vector-lib vector-fold vector-map))

  ;;;

  (define-generic (fmap f v))

  (define-method (fmap (f #t) (ls <list>))
    (map f ls))

  (define-method (fmap (f #t) (vec <vector>))
    (vector-map (lambda (_ v) (f v)) vec))

  (define (walk inner outer ast)
    (outer (fmap inner ast)))

  (define (postwalk f ast)
    (walk (cute postwalk f <>) f ast))

  (define (prewalk f ast)
    (walk (cute prewalk f <>) identity (f ast)))

  (define-generic (fold f v coll))

  (define-method (fold (f #t) (v #t) (ls <list>))
    (foldl f v ls))

  (define-method (fold (f #t) (v #t) (vec <vector>))
    (vector-fold (lambda (_ acc v) (f acc v)) v vec))

  ;; (define-generic (fold-st f iv ist coll))

  ;; (define-method (fold-st (f #t) (iv #t) (ist #t) (ls <list>))
  ;;   (define (fold-st-ls ls v st)
  ;;     (if (null? ls)
  ;;       (values v st)
  ;;       (receive (v* st*) (f v st (car ls))
  ;;         (fold-st-ls (cdr ls) v* st*))))
  ;;   (fold-st-ls ls iv ist))

  (define-generic (fmap-st f coll))

  (define-method (fmap-st (f #t) (coll <vector>) ist)
    (let* ((len (vector-length coll))
           (res (make-vector len))
           (st ist))
      (do ((i 0 (add1 i))) ((= i len))
        (receive (v st*) (f (vector-ref coll i) st)
          (vector-set! res i v)
          (set! st st*)))
      (values res st)))

  ;;;

  (define-syntax defrecord
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
       
       (define (setter recname classname slotname)
         `(,(r 'define-method) ((setter ,(symbol-append '|.| slotname))
                                (,(r 'rec) ,classname)
                                (,(r 'val) #t))
           (,(r 'set!) (,(rec-accessorize recname slotname) ,(r 'rec)) ,(r 'val))))
       
       (let* ((recname (caadr form))
              (classname (classify recname))
              (slotnames (cdadr form))
              (super (if (= (length form) 3)
                       (caddr form)
                       (r '<primitive-object>))))
         `(,(r 'begin)
           ;; The record type (for constructor, pattern matching)
           (,(r 'define-record-type) ,recname
            (,recname ,@slotnames) ,(predicativize recname)
            ,@(map (lambda (slotname)
                     (let ((axor (rec-accessorize recname slotname)))
                     `(,slotname ,axor (setter ,axor))))
                   slotnames))
           ;; The corresponding class (for dispatch):
           (,(r 'define-primitive-class) ,classname (,super)
            ,(predicativize recname))
           ;; Dot-functions (for nicer field access):
           ,@(apply append
                    (map (lambda (slotname)
                           (list (accessor recname classname slotname)
                                 (setter recname classname slotname)))
                         slotnames)))))))
  
  (define-syntax define-enum
    (er-macro-transformer
     (lambda (form r _)
       (define (classify recname)
         (symbol-append '< recname '>))
       
       (let* ((abstract (cadr form))
              (variants (cddr form))
              (abstract-class (classify abstract)))
         `(,(r 'begin)
            (,(r 'define-class) ,abstract-class) ; abstract supertype
            ,@(map                               ; variants/subtypes
               (lambda (variant) `(defrecord ,variant ,abstract-class))
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


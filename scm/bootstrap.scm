(module centring.bootstrap
  (def
   match
   defenum
   -defvariant
   defmethod

   symbol
   name
   writeln)

  ;;; FIXME: Macros aren't fully hygienic and client code needs to `(use coops)`

  (import scheme chicken)
  (use (rename matchable (match orig-match))
       (only extras printf)
       coops coops-primitive-objects
       irregex
       (only miscmacros define-syntax-rule))

  (import-for-syntax scheme chicken)
  (use-for-syntax (only (srfi 1) split-at append-map)
                  (only (srfi 13) string-join)
                  (only extras sprintf)
                  (only coops define-class))

  (define-syntax def
    (ir-macro-transformer
      (lambda (exp _ _)
        `(define ,@(cdr exp)))))

  (define-for-syntax (anglify . syms)
    (string->symbol (sprintf "<~A>" (string-join (map symbol->string syms) "/"))))

  (define-syntax -defvariant
    (er-macro-transformer
      (lambda (exp rename _)
        (let* ((super (cadr exp))
               (name (caaddr exp))
               (class (anglify super name))
               (ctor-s (string-append (symbol->string super) "/" 
                                      (symbol->string name)))
               (ctor (string->symbol ctor-s))
               (pred (string->symbol (string-append ctor-s "?")))
               (pred-param (rename 'v))
               (fields (cdaddr exp)))
          `(begin
             (define-class ,class (,(anglify super)) ,fields)
             (def (,ctor ,@fields)
               (make ,(anglify super name) 
                     ,@(append-map (lambda (field) `((quote ,field) ,field))
                                   fields)))
             (def (,pred ,pred-param) (eq? (class-of ,pred-param) ,class)))))))

  (define-syntax defenum
    (er-macro-transformer
      (lambda (exp _ _)
        `(begin
           (define-class ,(anglify (cadr exp)))
           ,@(map (lambda (varspec) `(-defvariant ,(cadr exp) ,varspec))
                  (cddr exp))))))

  (define-for-syntax (keyword->symbol kw)
    (string->symbol (keyword->string kw)))

  (define-for-syntax (convert-formals formals)
    (let recur ((res '()) (formals formals))
      (cond 
        ((null? formals) (reverse res))
        ((keyword? (car formals))
         (recur (cons `(,(car res) ,(anglify (keyword->symbol (car formals))))
                      (cdr res))
                (cdr formals)))
        (else (recur (cons (car formals) res) (cdr formals))))))

  (define-syntax defmethod
    (er-macro-transformer
      (lambda (expr _ _)
        `(define-method (,(caadr expr) ,@(convert-formals (cdadr expr)))
           ,@(cddr expr)))))

  (define-for-syntax (partition n ls)
    (let recur ((res '())
                (rem ls))
      (if (< (length rem) n)
        (reverse res)
        (receive (prefix suffix) (split-at rem n)
          (recur (cons prefix res) suffix)))))

  (define-syntax match
    (ir-macro-transformer
      (lambda (exp _ _)
        `(orig-match ,(cadr exp)
           ,@(partition 2 (cddr exp))))))

  (def (writeln v)
    (write v)
    (write-char #\newline))

  (def (symbol . args)
    (match args
      `(,ns ,name) (string->symbol (string-append ns "/" name))
      `(,name)     (string->symbol name)))

  (def slash-re (sre->irregex "/")) 

  (define-method (name (sym <symbol>))
    (let ((str (symbol->string sym)))
      (if (eq? (string-ref str 0) #\/)
        str
        (match (irregex-split slash-re str)
          `(,_ ,name) name
          `(,name)     name)))))

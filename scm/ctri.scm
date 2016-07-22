(use matchable
     sequences
     (only extras pretty-print)
     (only irregex irregex-split)
     (only posix current-directory)
     (only pathname-expand pathname-expand)
     (only anaphora aif acond awhile)
     (only linenoise linenoise history-add)
     (only clojurian-syntax ->)
     (only miscmacros unless)
     args

     (only centring.util literal? try)
     (prefix centring.expand exp:)
     (prefix centring.analyze ana:)
     (prefix centring.ast ast:)
     (prefix centring.cek cek:)
     (prefix centring.env env:)
     (prefix centring.value val:))

;;;;

(define (ctr->scm v)
  ;; TODO: print records, tuples etc. better
  (match v
    (#(#(_ type-name) fields ...)
     (cons (ctr->scm type-name) (smap '() ctr->scm fields)))
    (($ FnClosure formal _ _ _)
     `(Fn))
    ((? val:Continuation?)
     `(Cont))
    ((? val:Symbol?)
     (aif (val:Symbol-ns v)
       (symbol-append it '/ (val:Symbol-name v))
       (val:Symbol-name v)))
    ((? literal?) v)
    (_ (error "ctr->scm: unimplemented conversion" v))))

(define (init! options)
  (unless (or (assq 'esxp options) (assq 'ana options))
    (env:ctr-path
     (aif (assq 'path options)
       (map pathname-expand (irregex-split #\: (cdr it)))
       (list (current-directory))))
    (env:ns-extend! (env:ns-ref 'ctr.lang) 'Tuple #f)
    (-> '(require ctr.lang)
        exp:expand-all
        ana:analyze
        cek:interpret)
    (env:current-ns (env:ns-ref 'ctr.user))))

(define (make-action options)
  (cond
   ((assq 'esxp options)
    (o pretty-print exp:expand-all))
   ((assq 'ana options)
    (o pretty-print ast:ast->sexp
       ana:analyze exp:expand-all))
   (else
    (o print ctr->scm
       cek:interpret ana:analyze exp:expand-all))))

(define (repl action)
  (let ((prompt (lambda () (sprintf "~S> " (env:Ns-name (env:current-ns)))))
        (get-type (condition-property-accessor 'ctr 'type))
        (get-message (condition-property-accessor 'ctr 'message)))
    (awhile (linenoise (prompt))
      (history-add it)
      (try
       (-> it open-input-string read action)
       (catch exn
         (fprintf (current-error-port) "~A: ~S~%"
                  (get-type exn)
                  (get-message exn)))))))

;;;; Reader Setup

(keyword-style #:prefix)

(define ((read-ctor ctor-sym end-char) port)
  (let loop ((c (peek-char port)) (exprs '()))
    (cond
     ((eof-object? c) (error "EOF reached while parsing #(...)!"))
     ((char=? c end-char)
      (read-char port)
      `(,ctor-sym ,@(reverse exprs)))
     ((char-whitespace? c)
      (read-char port)
      (loop (peek-char port) exprs))
     (else
      (let ((expr (read port)))
        (loop (peek-char port) (cons expr exprs)))))))

(set-sharp-read-syntax! #\( (read-ctor 'ctr.lang/Tuple #\)))

;;;; Main & Option Parsing

(define opts
  (list (args:make-option (esxp) none: "Just expand S-expr.")
        (args:make-option (ana) none: "Just build and print AST.")

        (args:make-option (e) (required: "EXPR") "Use EXPR as input.")

        (args:make-option (path) (required: "PATH")
                          "Use PATH (colon-separated) as the CTR_PATH")

        (args:make-option (h help) none: "Display this text.")))

(define (main arglist)
  (let-values (((options operands) (args:parse arglist opts)))
    (init! options)
    (acond
     ((pair? operands)
      ((make-action options) `(do ,@(read-file (car operands)))))
     ((assq 'e options)
      ((make-action options) (read (open-input-string (cdr it)))))
     ((assq 'h options)
      (print (args:usage opts)))
     (else (repl (make-action options))))
    (exit 0)))

(main (command-line-arguments))

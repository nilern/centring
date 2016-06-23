(use (only extras pretty-print)
     (only irregex irregex-split)
     (only posix current-directory)
     (only pathname-expand pathname-expand)
     (only clojurian-syntax ->>)
     (only anaphora aif acond awhile)
     (only linenoise linenoise history-add)
     (only data-structures o)
     persistent-hash-map
     args
     
     (only centring.expand expand-all)
     (prefix centring.ast ast:)
     (only centring.ast ast->sexp)
     (only centring.analyze analyze alphatize&specialize dnf-convert)
     (prefix centring.cps cps:)
     (only centring.rt make-Fiber Fiber-curr-ns)
     (only centring.interpret eval-cps)
     (only centring.ns Ns-name))

;;;;

(define optimize-cps cps:eta-contract)

(define (make-action options path)
  (cond
   ((assq 'esxp options)
    expand-all)
   ((assq 'iana options)
    (o ast->sexp analyze expand-all))
   ((assq 'fana options)
    (o ast->sexp
       dnf-convert
       (cute alphatize&specialize 'centring.user <>)
       analyze expand-all))
   ((assq 'icps options)
    (o ast->sexp
       cps:convert
       dnf-convert
       (cute alphatize&specialize 'centring.user <>)
       analyze expand-all))
   ((assq 'fcps options)
    (o ast->sexp
       optimize-cps
       cps:convert
       dnf-convert
       (cute alphatize&specialize 'centring.user <>)
       analyze expand-all))
   (else
    (o (cute eval-cps (make-Fiber) <>)
       optimize-cps
       cps:convert
       dnf-convert
       (cute alphatize&specialize 'centring.user <>)
       analyze expand-all))))

(define (repl path)
  (let* ((itp (make-Fiber))
         (prompt (lambda () (sprintf "~S> " (Ns-name (Fiber-curr-ns itp)))))
         (get-message (condition-property-accessor 'exn 'message))
         (get-arguments (condition-property-accessor 'exn 'arguments)))
    (awhile (linenoise (prompt))
      (history-add it)
      (handle-exceptions exn
        (fprintf (current-error-port) "Error: ~A: ~S~%"
                 (get-message exn)
                 (get-arguments exn))
        (->> (read (open-input-string it))
             expand-all
             analyze
             (alphatize&specialize 'centring.user)
             dnf-convert
             ((cute cps:cps-k <>
                    (lambda (v) (ast:Primop 'halt (vector v) #()))))
             optimize-cps
             (eval-cps itp)
             (printf "~S~%"))))))

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

(set-sharp-read-syntax! #\( (read-ctor 'centring.lang/Tuple #\)))

;;;; Main & Option Parsing

(define opts
  (list (args:make-option (esxp) none: "Just expand S-expr.")
        (args:make-option (iana) none: "Just build and print AST.")
        (args:make-option (fana) none: "Just build, alphatize and print AST.")
        (args:make-option (icps) none: "Just CPS convert.")
        (args:make-option (fcps) none: "CPS convert and optimize.")

        (args:make-option (e) (required: "EXPR") "Use EXPR as input.")

        (args:make-option (path) (required: "PATH")
                          "Use PATH (colon-separated) as the CTR_PATH")

        (args:make-option (h help) none: "Display this text.")))

(define (main arglist)
  (receive (options operands) (args:parse arglist opts)
    (let ((ctr-path (aif (assq 'path options)
                      (map pathname-expand (irregex-split #\: (cdr it)))
                      (list (current-directory)))))
      (acond
       ((pair? operands)
        (pretty-print
         ((make-action options ctr-path) `(do ,@(read-file (car operands))))))
       ((assq 'e options)
        (pretty-print
         ((make-action options ctr-path) (read (open-input-string (cdr it))))))
       ((assq 'h options)
        (print (args:usage opts)))
       (else (repl ctr-path)))))
  (exit 0))

(main (command-line-arguments))

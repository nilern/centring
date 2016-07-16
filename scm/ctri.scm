(use (only extras pretty-print)
     (only irregex irregex-split)
     (only posix current-directory)
     (only pathname-expand pathname-expand)
     (only anaphora aif acond awhile)
     (only linenoise linenoise history-add)
     args
     
     (prefix centring.expand exp:)
     (prefix centring.analyze ana:)
     (prefix centring.ast ast:)
     (prefix centring.dispatch dis:)
     (prefix centring.cek cek:))

;;;;

(define (make-action options path)
  (cond
   ((assq 'esxp options)
    exp:expand-all)
   ((assq 'ana options)
    (o ast:ast->sexp
       ana:analyze exp:expand-all))
   (else
    (o cek:interpret ana:analyze exp:expand-all))))

(define (repl path)
  (error "REPL unimplemented"))
  ;; (let* ((itp (make-Fiber))
  ;;        (prompt (lambda () (sprintf "~S> " (Ns-name (Fiber-curr-ns itp)))))
  ;;        (get-message (condition-property-accessor 'exn 'message))
  ;;        (get-arguments (condition-property-accessor 'exn 'arguments)))
  ;;   (awhile (linenoise (prompt))
  ;;     (history-add it)
  ;;     (handle-exceptions exn
  ;;       (fprintf (current-error-port) "Error: ~A: ~S~%"
  ;;                (get-message exn)
  ;;                (get-arguments exn))
  ;;       (->> (read (open-input-string it))
  ;;            expand-all
  ;;            analyze
  ;;            (alphatize&specialize 'centring.user)
  ;;            dnf-convert
  ;;            ((cute cps:cps-k <>
  ;;                   (lambda (v) (ast:Primop 'halt (vector v) #()))))
  ;;            optimize-cps
  ;;            (eval-cps itp)
  ;;            (printf "~S~%"))))))

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
        (args:make-option (ana) none: "Just build and print AST.")

        (args:make-option (e) (required: "EXPR") "Use EXPR as input.")

        (args:make-option (path) (required: "PATH")
                          "Use PATH (colon-separated) as the CTR_PATH")

        (args:make-option (h help) none: "Display this text.")))

(define (main arglist)
  (receive (options operands) (args:parse arglist opts)
    (let ((ctr-path (aif (assq 'path options)
                      (map pathname-expand (irregex-split #\: (cdr it)))
                      (list (current-directory)))))
      (cek:ctr-path ctr-path)
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

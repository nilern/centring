(use (only extras pretty-print)
     (only irregex irregex-split)
     (only posix current-directory)
     (only pathname-expand pathname-expand)
     (only clojurian-syntax ->>)
     (only anaphora aif acond awhile)
     (only linenoise linenoise history-add)
     (only data-structures o)
     args
     
     (only centring.expand expand-all)
     (only centring.analyze analyze alphatize&specialize ast->sexp)
     (only centring.eval-ast make-interpreter eval-ast .curr-ns)
     (only centring.ns Ns-name))

(keyword-style #:prefix)

(define opts
  (list (args:make-option (esxp) none: "Just expand S-expr.")
        (args:make-option (iana) none: "Just build and print AST.")
        (args:make-option (fana) none: "Just build, alphatize and print AST.")

        (args:make-option (e) (required: "EXPR") "Use EXPR as input.")

        (args:make-option (path) (required: "PATH")
                          "Use PATH (colon-separated) as the CTR_PATH")

        (args:make-option (h help) none: "Display this text.")))

(define (make-action options path)
  (cond
   ((assq 'esxp options)
    expand-all)
   ((assq 'iana options)
    (o ast->sexp analyze expand-all))
   ((assq 'fana options)
    (o ast->sexp
       (cute alphatize&specialize 'centring.user <>) analyze expand-all))
   (else
    (o (cute eval-ast (make-interpreter path) <>)
       analyze expand-all))))

(define (repl path)
  (let* ((itp (make-interpreter path))
         (prompt (lambda () (sprintf "~S> " (Ns-name (.curr-ns itp)))))
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
             (eval-ast itp)
             (printf "~S~%"))))))

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

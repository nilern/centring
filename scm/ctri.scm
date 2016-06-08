(use (only extras pretty-print)
     (only irregex irregex-split)
     (only posix current-directory)
     (only pathname-expand pathname-expand)
     (only clojurian-syntax ->>)
     (only anaphora aif acond awhile)
     (only linenoise linenoise history-add)
     args
     
     (only centring.expand expand-all)
     (only centring.analyze analyze ast->sexp)
     (only centring.eval-ast make-interpreter eval-ast .curr-ns)
     (only centring.ns Ns-name))

(keyword-style #:prefix)

(define opts
  (list (args:make-option (esxp) :none "Just expand S-expr.")
        (args:make-option (iana) :none "Just build and print AST.")

        (args:make-option (e) (:required "EXPR") "Use EXPR as input.")

        (args:make-option (path) (:required "PATH")
                          "Use PATH (colon-separated) as the CTR_PATH")

        (args:make-option (h help) :none "Display this text.")))

(define (batch sexp options)
  (cond
   ((assq 'esxp options)
    (->> sexp expand-all pretty-print))
   ((assq 'iana options)
    (->> sexp expand-all analyze ast->sexp pretty-print))
   (else
    (->> sexp expand-all analyze (eval-ast (make-interpreter)) pretty-print))))

(define (repl)
  (let* ((itp (make-interpreter))
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
        (batch `(do ,@(read-file (car operands))) options))
       ((assq 'e options)
        (batch (read (open-input-string (cdr it))) options))
       ((assq 'h options)
        (print (args:usage opts)))
       (else (repl)))))
  (exit 0))

(main (command-line-arguments))

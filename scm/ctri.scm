(use matchable
     (only extras read-file pretty-print)
     (only data-structures o)
     (only ports with-input-from-string)

     (prefix centring.expand exp:)
     (prefix centring.analyze ana:))

(keyword-style #:prefix)

(define (main arglist)
  (let* ((sexp (match (cdr arglist)
                (("-e" estr) (with-input-from-string estr read))
                ((filename)  `(do ,@(read-file filename)))
                (_ (exit 1))))
         (analyze (o ana:analyze exp:expand-all)))
    (match (car arglist)
      ("--esxp" (pretty-print (exp:expand-all sexp)))
      ("--iana" (pretty-print (ana:ast->sexpr (analyze sexp)))))))

#+compiling
(main (command-line-arguments))

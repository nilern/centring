(use (only matchable match)
     (only extras read-file pretty-print)
     (only data-structures o)
     (only ports with-input-from-string)

     (only centring.expand expand-all)
     (prefix centring.analyze ana:))

(define (main arglist)
  (let* ((sexp (match (cdr arglist)
                 (("-e" estr) (with-input-from-string estr read))
                 ((filename)  `(do ,@(read-file filename)))
                 (_ (exit 1))))
         (analyze (o ana:analyze expand-all))
         (alphanalyze (o ana:alphatize&specialize analyze)))
    (match (car arglist)
      ("--esxp" (pretty-print (expand-all sexp)))
      ("--iana" (pretty-print (ana:ast->sexp (analyze sexp))))
      ("--fana" (pretty-print (ana:ast->sexp (alphanalyze sexp)))))))

#+compiling
(main (command-line-arguments))

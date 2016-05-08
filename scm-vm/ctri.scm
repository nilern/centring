(use (only matchable match)
     (only extras read-file pretty-print)
     (only data-structures o)
     (only ports with-input-from-string)

     (only centring.expand expand-all)
     (prefix centring.analyze ana:)
     (prefix centring.cps cps:))

(keyword-style #:prefix)

(define (main arglist)
  (let* ((mainsym (gensym 'main))
         (sexp `(letfn (((,mainsym)
                         ,(match (cdr arglist)
                            (("-e" estr) (with-input-from-string estr read))
                            ((filename)  `(do ,@(read-file filename)))
                            (_ (exit 1)))))
                       ,mainsym))
         (analyze (o ana:analyze expand-all))
         (alphanalyze (o ana:alphatize&specialize analyze))
         (cps (o (lambda (cast)
                   (cps:cps-c cast
                              (lambda (v)
                                (cps:make-Primop 'halt (vector v) #() #f))))
                 alphanalyze)))
    (match (car arglist)
      ("--esxp" (pretty-print (expand-all sexp)))
      ("--iana" (pretty-print (ana:ast->sexp (analyze sexp))))
      ("--fana" (pretty-print (ana:ast->sexp (alphanalyze sexp))))
      ("--icps" (pretty-print (cps:cps->sexp (cps sexp)))))))

#+compiling
(main (command-line-arguments))

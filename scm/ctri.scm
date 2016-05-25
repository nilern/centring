(use matchable
     (only extras read-file pretty-print)
     (only data-structures o)
     (only ports with-input-from-string)
     dyn-vector

     (prefix centring.expand exp:)
     (prefix centring.analyze ana:)
     (prefix centring.cps cps:)
     (prefix centring.emit emit:)
     (prefix centring.value val:)
     (prefix centring.vm vm:))

(keyword-style #:prefix)

(define (main arglist)
  (let* ((sexp (match (cdr arglist)
                (("-e" estr) (with-input-from-string estr read))
                ((filename)  `(do ,@(read-file filename)))
                (_ (exit 1))))
         (analyze (o ana:analyze exp:expand-all))
         (alphanalyze (o (cute ana:alphatize&specialize 'centring.user <>)
                         analyze))
         (cps (o cps:ast->cps alphanalyze))
         (optimize (o cps:contify cps))
         (emit (o (lambda (cexp)
                    (emit:emit!
                     (emit:ProcBuilder 'main
                                       (make-dynvector 0 #f)
                                       (make-dynvector 0 #f)
                                       #f
                                       (make-dynvector 0 #f))
                     cexp))
                  optimize)))
    (match (car arglist)
      ("--esxp" (pretty-print (exp:expand-all sexp)))
      ("--iana" (pretty-print (ana:ast->sexpr (analyze sexp))))
      ("--fana" (pretty-print (ana:ast->sexpr (alphanalyze sexp))))
      ("--icps" (pretty-print (cps:cps->sexpr (cps sexp))))
      ("--fcps" (pretty-print (cps:cps->sexpr (optimize sexp))))
      ("--asm"  (pretty-print (emit:procb->sexpr (emit sexp))))
      ("--run"
       (let ((proc (emit:assemble (emit sexp)))
             (fiber (vm:make-fiber (make-dynvector 0 (void))
                                   (make-dynvector 0 (void))
                                   #f 0
                                   #f)))
         (pretty-print (vm:run! fiber proc)))))))
                     

#+compiling
(main (command-line-arguments))

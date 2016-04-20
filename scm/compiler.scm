(include "expand.scm")
(include "coreast.scm")
(include "cps.scm")
 
(use (only matchable match)
     (only extras read-file pretty-print))

(import (only centring.expand ctr-expand-all)
        (prefix centring.coreast cast:)
        (prefix centring.cps cps:))

(keyword-style #:prefix)

(define (main arglist)
  (let* ((sexp (match (cdr arglist)
                 (("-e" estr) (with-input-from-string estr read))
                 ((filename)  `(do ,@(read-file filename)))
                 (_ (exit 1))))
         (analyze (o cast:analyze ctr-expand-all))
         (alphanalyze (o (cute cast:alphatize&specialize '() <>) analyze))
         (cps (o (lambda (cast)
                   (cps:cps-k cast
                              (lambda (v)
                                (cps:make-Primop 'halt `(,v) '() '()))))
                 alphanalyze))
         (optimize (o cps:remove-unuseds cps:beta-contract cps:eta-contract cps))
         (cconvert (o (cps:closure-convert '()) optimize))
         (compile (o cps:emit optimize)))
    (match (car arglist)
      ("--esxp" (pretty-print (ctr-expand-all sexp)))
      ("--icast" (pretty-print (cast:core->sexp (analyze sexp))))
      ("--fcast" (pretty-print (cast:core->sexp (alphanalyze sexp))))
      ("--icps" (pretty-print (cps:cps->sexp (cps sexp))))
      ("--fcps" (pretty-print (cps:cps->sexp (cconvert sexp))))
      ("--asm"  (cps:display-codeobj #t (compile sexp)))
      (_ (exit 2))))
  (exit 0))

#+compiling
(main (command-line-arguments))

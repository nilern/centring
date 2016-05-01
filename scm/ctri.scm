(use (only matchable match)
     (only clojurian-syntax ->>)
     (only centring.expand ctr-expand-all)

     array
     (prefix centring.coreast cast:)
     (prefix centring.cps cps:)
     (prefix centring.vm vm:))

(define (main arglist)
  (let* ((sexp (match arglist
                 (("-e" estr) (with-input-from-string estr read))
                 ((filename)  `(do ,@(read-file filename)))
                 (_ (exit 1))))
         (cps-convert
          (lambda (cast)
            (cps:cps-k cast (lambda (v)
                              (cps:make-Primop 'halt `(,v) '() '())))))
         (cexp (->> sexp
                    ctr-expand-all
                    cast:analyze (cast:alphatize&specialize '())
                    cps-convert
                    cps:eta-contract cps:beta-contract cps:remove-unuseds
                    (cps:closure-convert '() '()) car
                    (cps:prewalk cps:serialize-closes))))
    (printf "~S~%"
            (vm:run! (vm:make-fiber (make-array 2) 0 (vm:emit cexp) 0)))))

#+compiling
(main (command-line-arguments))

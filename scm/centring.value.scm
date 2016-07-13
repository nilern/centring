(module centring.value
  *

  (import scheme chicken)
  
  (use matchable
       data-structures
       (only miscmacros until)

       centring.util)

  ;;; Int:s are just fixnums

  ;;; Bool:s are just booleans

  ;;;; Symbol

  (defrecord (Symbol ns name))

  ;;;; FnClosure

  (define-record-type FnClosure
    (FnClosure formal df cases caseq)
    FnClosure?
    (formal FnClosure-formal)
    (df FnClosure-df)
    (cases FnClosure-cases)
    (caseq FnClosure-caseq (setter FnClosure-caseq)))

  (define (make-fn formal cases env)
    (let ((caseq (make-queue)))
      (doseq (case cases)
        (queue-add! caseq (vector (car case) (cdr case) env)))
      (FnClosure formal #f #() caseq))))

(module centring.value
  *

  (import scheme chicken)
  
  (use matchable
       data-structures
       dyn-vector
       (only miscmacros until)

       centring.util
       centring.env)

  ;;; Int:s are just fixnum:s

  ;;; Bool:s are just boolean:s

  ;;; Char:s are just char:s

  ;;; Records are vector:s with the 0th element the type

  ;;;; Byte Types

  (defrecord (BytesInstance type bytes))   

  ;;;; Symbol

  (defrecord (Symbol ns name))

  ;;;; FnClosure

  (define-record-type FnClosure
    (FnClosure formal df cases caseq)
    FnClosure?
    (formal FnClosure-formal)
    (df FnClosure-body (setter FnClosure-body))
    (cases FnClosure-cases)
    (caseq FnClosure-caseq (setter FnClosure-caseq)))

  (define (make-fn formal cases env)
    (let ((caseq (make-queue)))
      (doseq (case cases)
        (match-let (((cond . body) case))
          (doseq (clause cond)
            (queue-add! caseq (vector clause body env (current-ns))))))
      (FnClosure formal #f (make-dynvector 0 #f) caseq)))

  ;;;; Continuation

  (defrecord (Continuation k)))

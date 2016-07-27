(library (ctr util)
  (export ns-sep ctr-error)
  (import (rnrs (6)))

  (define ns-sep '/)

  (define-condition-type &ctr &condition make-ctr-error ctr-error?)

  (define (ctr-error msg . irritants)
    (raise
     (condition
      (make-ctr-error)
      (make-message-condition msg)
      (make-irritants-condition irritants)))))

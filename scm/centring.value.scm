(module centring.value
  *

  (import scheme chicken)
  (use data-structures)

  (define-record-type Closure
    (Closure arg vals names body methods mergeq)
    Closure?
    (arg Closure-arg)
    (vals Closure-vals)
    (names Closure-names)
    (body Closure-body)
    (methods Closure-methods)
    (mergeq Closure-mergeq))

  (define (make-closure arg methods clns clvs)
    (Closure arg clvs clns #f methods (make-queue)))

  (define (closure-merge! cls1 cls2)
    (queue-add! (Closure-mergeq cls1) cls2)))

  

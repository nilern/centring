(library (util queue)
  (export make-queue
          queue-empty? queue-peek
          queue-pop! enqueue!)
  (import (rnrs (6))
          (only (rnrs mutable-pairs (6)) set-cdr!))

  (define-record-type Queue
    (fields
     (mutable front)
     (mutable back)))

  (define (make-queue)
    (make-Queue '() '()))

  (define (queue-empty? q)
    (null? (Queue-front q)))

  (define (queue-peek q default)
    (let ((front (Queue-front q)))
      (if (null? front)
        default
        (car front))))

  (define (queue-pop! q default)
    (let ((front (Queue-front q)))
      (if (null? front)
        default
        (let ((front* (cdr front)))
          (Queue-front-set! q front*)
          (when (null? front*)
            (Queue-back-set! q front*))
          (car front)))))

  (define (enqueue! q v)
    (let ((back (Queue-back q)))
      (if (null? back)
        (let ((back* (cons v back)))
          (Queue-front-set! q back*)
          (Queue-back-set! q back*))
        (let ((back* (cons v (cdr back))))
          (set-cdr! back back*)
          (Queue-back-set! q back*))))))

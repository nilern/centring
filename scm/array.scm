(module array
   (make-array array?
    array-length array-ref array-set! array-index array-push!
    list->array array->list)

   (import scheme chicken)
   (use (only extras fprintf)
        (only vector-lib vector-for-each)
        (only miscmacros define-syntax-rule))

   (define-record-type array
     (make-array-raw length buffer)
     array?
     (length array-length (setter array-length))
     (buffer array-buffer (setter array-buffer)))

   (define-record-printer (array arr out)
     (display "#[" out)
     (when (> (array-length arr) 0)
       (write (array-ref arr 0) out)
       (do ((i 1 (add1 i))) ((= i (array-length arr)))
         (fprintf out " ~S" (array-ref arr i))))
     (display "]" out))

   (define-syntax-rule (bounds-check arr i)
     (assert (< -1 i (array-length arr)) "out of range" arr i))

   (define (make-array n #!optional fill)
     (make-array-raw 0 (if fill (make-vector n fill) (make-vector n))))

   (define (array-ref arr i)
     (bounds-check arr i)
     (vector-ref (array-buffer arr) i))

   (define (array-set! arr i v)
     (bounds-check arr i)
     (vector-set! (array-buffer arr) i v))

   (define (array-grow arr)
     (let* ((oldbuf (array-buffer arr))
            (oldlen (vector-length oldbuf))
            (newbuf (make-vector (if (> oldlen 0) (* oldlen 2) 8))))
       (do ((i 0 (add1 i))) ((= i (vector-length oldbuf)))
         (set! (vector-ref newbuf i) (vector-ref oldbuf i)))
       (set! (array-buffer arr) newbuf)))

   (define (array-index pred arr)
     (let recur ((i 0))
       (cond
        ((>= i (array-length arr)) #f)
        ((pred (array-ref arr i)) i)
        (else (recur (add1 i))))))

   (define (array-push! arr v)
     (when (= (vector-length (array-buffer arr)) (array-length arr))
       (array-grow arr))
     (set! (vector-ref (array-buffer arr) (array-length arr)) v)
     (set! (array-length arr) (add1 (array-length arr)))
     (array-length arr))

   (define (list->array ls)
     (let ((arr (make-array 0)))
       (for-each (lambda (v) (array-push! arr v)) ls)
       arr))

   (define (array->list arr)
     (let ((res '()))
       (do ((i (sub1 (array-length arr)) (sub1 i))) ((= -1 i))
         (set! res (cons (array-ref arr i) res)))
       res)))
           
       
       

(module array
   (make-array array array?
    array-length array-ref array-set! array-index array-push! array-pop!
    array-append! array-split-off! array-merge-with! array-merge-with
    array-clone list->array array->list)

   (import scheme chicken)
   (use (only extras fprintf)
        (only vector-lib vector-for-each)
        (only miscmacros define-syntax-rule)
        (only clojurian-syntax doto))

   ;;; FIXME: Clean out values when array is shortened

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

   (define (array . vals)
     (list->array vals))

   (define (array-ref arr i)
     (bounds-check arr i)
     (vector-ref (array-buffer arr) i))

   (define (array-set! arr i v)
     (bounds-check arr i)
     (vector-set! (array-buffer arr) i v))

   (define (array-grow arr)
     (let* ((oldbuf (array-buffer arr))
            (oldlen (vector-length oldbuf))
            (newbuf (make-vector (if (> oldlen 0) (* oldlen 2) 2))))
       (do ((i 0 (add1 i))) ((= i (vector-length oldbuf)))
         (set! (vector-ref newbuf i) (vector-ref oldbuf i)))
       (set! (array-buffer arr) newbuf)))

   (define (array-index pred arr)
     (let recur ((i 0))
       (cond
        ((>= i (array-length arr)) #f)
        ((pred (array-ref arr i)) i)
        (else (recur (add1 i))))))

   (define (array-merge-with! f arr1 arr2)
     (let* ((len1 (array-length arr1))
            (len2 (array-length arr2))
            (end (min len1 len2)))
       (do ((i 0 (add1 i))) ((= i end))
         (array-set! arr1 i (f (array-ref arr1 i) (array-ref arr2 i))))
       (when (> len2 len1)
         (do ((i len1 (add1 i))) ((= i len2))
           (array-push! arr1 (array-ref arr2 i))))
       arr1))

   (define (array-merge-with f arr1 arr2)
     (array-merge-with! f (array-clone arr1) arr2))

   (define (array-push! arr v)
     (when (= (vector-length (array-buffer arr)) (array-length arr))
       (array-grow arr))
     (set! (vector-ref (array-buffer arr) (array-length arr)) v)
     (set! (array-length arr) (add1 (array-length arr)))
     (array-length arr))

   (define (array-pop! arr)
     (let* ((len (array-length arr))
            (newlen (sub1 len)))
       (if (> len 0)
         (let ((v (array-ref arr newlen)))
           (set! (array-length arr) newlen)
           v)
         (error "tried to pop empty array!"))))

   (define (array-append! arr1 arr2)
     (do ((i 0 (add1 i))) ((= i (array-length arr2)))
       (array-push! arr1 (array-ref arr2 i)))
     (array-length arr1))

   (define (array-split-off! arr n)
     (let ((split (- (array-length arr) n))
           (narr (make-array n)))
       (do ((i split (add1 i))) ((= i (array-length arr)))
         (array-push! narr (array-ref arr i)))
       (set! (array-length arr) split)
       narr))

   (define (array-clone arr)
     (doto (make-array (array-length arr))
       (array-append! arr)))

   (define (list->array ls)
     (let ((arr (make-array 0)))
       (for-each (lambda (v) (array-push! arr v)) ls)
       arr))

   (define (array->list arr)
     (let ((res '()))
       (do ((i (sub1 (array-length arr)) (sub1 i))) ((= -1 i))
         (set! res (cons (array-ref arr i) res)))
       res)))
           
       
       

(import (rnrs (6))
        (only (chezscheme) format)

        (util collections)
        (util dynvector))

(define (parse-opts arglist)
  (define (key? s)
    (char=? (string-ref s 0) #\-))
  
  (define (parse-arg! k args)
    (if (or (null? args) (key? (first args)))
      (begin
        (hashtable-set! kvs k #t)
        (parse! args))
      (begin
        (hashtable-set! kvs k (first args))
        (parse! (rest args)))))
  
  (define (parse! args)
    (cond
     ((empty? args)
      (values kvs sq))
     ((key? (first args))
      (parse-arg! (first args) (rest args)))
     (else
      (dynvector-push! sq (first args))
      (parse! (rest args)))))
  
  (define kvs (make-hashtable string-hash string=?))
  (define sq (make-dynvector))
  (parse! (rest arglist)))

(define (main arglist)
  (let*-values (((kvs sq) (parse-opts arglist))
                ((ks vs) (hashtable-entries kvs)))
    (format #t "~S ~S~%" ks vs)
    (format #t "~S~%" sq)))

(main (command-line))

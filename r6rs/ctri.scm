(import (rnrs (6))
        (only (chezscheme) open-input-string pretty-print format)

        (only (util) comp)
        (util collections)
        (util dynvector)

        (prefix (ctr expand) exp:))

;;;; CLI Option Handling

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

(define (make-action options)
  (cond
   ((hashtable-ref options "--esxp" #f)
    (comp pretty-print exp:expand-all))
   (else
    pretty-print)))

;;;; Main

(define (main arglist)
  (let*-values (((kvs sq) (parse-opts arglist))
                ((ks vs) (hashtable-entries kvs)))
    (cond
     ((hashtable-ref kvs "-e" #f)
      ((make-action kvs)
       (read (open-input-string (hashtable-ref kvs "-e" #f))))))))
    
(main (command-line))

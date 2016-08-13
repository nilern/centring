(import (rnrs (6))
        (only (chezscheme) open-input-string pretty-print format)

        (only (util) comp string-split)
        (util collections)
        (util dynvector)

        (only (ctr util) ctr-path)
        (only (ctr ast) ast->sexp)
        (only (ctr expand) expand-all)
        (only (ctr analyze) analyze resolve!)
        (only (ctr cek) interpret)
        (only (ctr read) ParseError? ParseError-msg ctr-read-all))

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
   ((hashtable-ref options "--fana" #f)
    (comp pretty-print ast->sexp
          resolve! analyze
          expand-all))
   ((hashtable-ref options "--iana" #f)
    (comp pretty-print ast->sexp
          analyze
          expand-all))
   ((hashtable-ref options "--esxp" #f)
    (comp pretty-print
          expand-all))
   (else
    (comp pretty-print
          interpret
          resolve! analyze
          expand-all))))

;;;; Main

(define (main arglist)
  (let*-values (((kvs sq) (parse-opts arglist))
                ((ks vs) (hashtable-entries kvs)))
    (ctr-path (string-split #\: (hashtable-ref kvs "--path" "")))
    (let ((port (if (hashtable-ref kvs "-e" #f)
                  (open-input-string (hashtable-ref kvs "-e" #f))
                  (open-file-input-port (dynvector-ref sq 0)
                                        (file-options)
                                        (buffer-mode block)
                                        (native-transcoder)))))
      (let-values (((res _) (ctr-read-all port)))
        (if (ParseError? res)
          (format (current-error-port) "ParseError: ~S"
                  (ParseError-msg res))
          ((make-action kvs) `(do ,@res)))))))

(main (command-line))

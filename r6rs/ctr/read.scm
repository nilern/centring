(library (ctr read)
  (export ctr-read)
  (import (rnrs (6))
          (only (chezscheme) format)

          (only (util) if-let doto comp))

  ;;;; PortSeq

  (define-record-type PortSeq
    (fields
     (immutable port)
     (immutable pos)))

  (define (make-pseq port)
    (make-PortSeq port (port-position port)))

  (define (pseq-synchronize! pseq)
    (set-port-position! (PortSeq-port pseq) (PortSeq-pos pseq)))

  (define (pseq-eof? pseq)
    (pseq-synchronize! pseq)
    (port-eof? (PortSeq-port pseq)))

  (define (pseq-first pseq)
    (pseq-synchronize! pseq)
    (lookahead-char (PortSeq-port pseq)))

  (define (pseq-rest pseq)
    (let ((port (PortSeq-port pseq)))
      (pseq-synchronize! pseq)
      (read-char port)
      (let ((pos* (port-position port)))
        (make-PortSeq port pos*))))

  ;;;; ParseError

  (define-record-type ParseError
    (fields
     (immutable msg)))

  ;;;; Primitive Parsers

  (define (mreturn v)
    (lambda (pseq)
      (values v pseq)))

  (define (fail err)
    (lambda (pseq)
      (values err pseq)))

  (define (any-char pseq)
    (if (pseq-eof? pseq)
      (values (make-ParseError "EOF reached.") pseq)
      (values (pseq-first pseq) (pseq-rest pseq))))

  (define (sat pred?)
    (mlet ((c any-char))
      (if (pred? c)
        (mreturn c)
        (fail
         (make-ParseError (format "~S did not satisfy ~S." c pred?))))))

  (define (tabular tab)
    (mlet ((c any-char))
      (if-let (p (hashtable-ref tab c #f))
        p
        (fail
         (make-ParseError (format "No readtable entry for ~S." c))))))
        
  ;;;; Combinators

  (define (mbind p f)
    (lambda (pseq)
      (let-values (((res pseq*) (p pseq)))
        (if (ParseError? res)
          (values res pseq*)
          ((f res) pseq*)))))

  (define-syntax mlet
    (syntax-rules ()
      ((mlet () body ...)
       (begin body ...))
      ((mlet ((v p) vps ...) body ...)
       (mbind p (lambda (v) (mlet (vps ...) body ...))))))

  (define (mplus p q)
    (lambda (pseq)
      (let-values (((p-res pseq*) (p pseq)))
        (if (ParseError? p-res)
          (q pseq)
          (values p-res pseq*)))))

  (define (fmap f p)
    (mlet ((a p))
      (mreturn (f a))))

  (define (many* p)
    (mplus (many+ p) (mreturn '())))

  (define (many+ p)
    (mlet ((a p)
           (as (many* p)))
      (mreturn (cons a as))))

  (define (sep-by* sep p)
    (mplus (sep-by+ sep p) (mreturn '())))

  (define (sep-by+ sep p)
    (mlet ((a p)
           (as (many* (mlet ((_ sep)) p))))
      (mreturn (cons a as))))
  
  ;;;; Read Table

  (define readtable
    (doto (make-eqv-hashtable)
      (hashtable-set! #\a (mreturn 1))
      (hashtable-set! #\b (mreturn 2))))

  ;;;; Read

  (define digit (sat char-numeric?))
  (define int (fmap (comp string->number list->string)
                    (many+ digit)))

  (define letter (sat char-alphabetic?))
  (define sym (fmap (comp string->symbol list->string)
                    (many+ letter)))

  (define expr
    (mplus int sym))

  (define ctr-read
    (case-lambda
     (()
      (ctr-read (current-input-port)))
     ((port) 
      (expr (make-pseq port))))))

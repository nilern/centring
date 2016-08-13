(library (ctr read)
  (export ParseError? ParseError-msg ctr-read ctr-read-all)
  (import (rnrs (6))
          (only (chezscheme) format)

          (only (util)
                if-let doto
                string-index
                some-fn complement partial comp))

  ;;; TODO: optimize

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

  (define (char c)
    (sat (partial eqv? c)))

  (define (one-of cs)
    (sat (lambda (c) (string-index c cs))))

  (define (none-of cs)
    (sat (lambda (c) (not (string-index c cs)))))

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

  (define (mplus . parsers)
    (lambda (pseq)
      (let recur ((ps parsers))
        (if (null? ps)
          (values (make-ParseError
                   (format "None of the parsers ~S matched." parsers))
                   pseq)
          (let-values (((p-res pseq*) ((car ps) pseq)))
            (if (ParseError? p-res)
              (recur (cdr ps))
              (values p-res pseq*)))))))

  (define (fmap f p)
    (mlet ((a p))
      (mreturn (f a))))

  (define (maybe p)
    (mplus p (mreturn #f)))

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

  (define (between start end p)
    (mlet ((_ start)
           (a p)
           (_ end))
      (mreturn a)))

  (define (surr-by surr p)
    (between surr surr p))
  
  ;;;; Read Table

  (define readtable (make-eqv-hashtable))

  (define sharptable (make-eqv-hashtable))

  ;;;; Read

  (define ws-char (sat char-whitespace?))
  (define comment (mlet ((_ (char #\;))
                         (_ (many* (sat (lambda (c)
                                          (not (or (eqv? c #\newline)
                                                   (eqv? c #\return))))))))
                    (mreturn #f)))
  (define ws (many+ (mplus ws-char
                           comment)))

  (define digit (sat char-numeric?))
  (define int (fmap (comp string->number list->string)
                    (many+ digit)))

  (define symchar (sat (complement
                        (some-fn char-whitespace?
                                 (lambda (c) (string-index c "()#;"))))))
  (define sym (fmap (comp string->symbol list->string)
                    (many+ symchar)))

  (define expr
    (surr-by (maybe ws)
             (mplus (tabular readtable)
                    int
                    sym)))

  (define ctr-read
    (case-lambda
     (()
      (ctr-read (current-input-port)))
     ((port)
      (expr (make-pseq port)))))

  (define ctr-read-all
    (case-lambda
     (()
      (ctr-read-all (current-input-port)))
     ((port)
      (let ((pseq (make-pseq port)))
        (let recur ((res '())
                    (pseq pseq))
          (if (pseq-eof? pseq)
            (values res pseq)
            (let-values (((p-res pseq*) (expr pseq)))
              (if (ParseError? p-res)
                (values p-res pseq*)
                (recur `(,@res ,p-res) pseq*)))))))))

  ;;;; Init

  (doto readtable
    (hashtable-set! #\(
                    (mlet ((es (many* expr))
                           (_ (char #\))))
                          (mreturn es)))
    (hashtable-set! #\' (fmap (lambda (e) `(quote ,e)) expr))
    (hashtable-set! #\# (tabular sharptable)))
                    
  (doto sharptable
    (hashtable-set! #\(
                    (mlet ((es (many* expr))
                           (_ (char #\))))
                      (mreturn (append '(ctr.lang/new ctr.lang/Tuple)
                                       es))))
    (hashtable-set! #\t (mreturn #t))
    (hashtable-set! #\f (mreturn #f))))

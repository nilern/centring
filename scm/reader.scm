(include "bootstrap.scm")

(module centring.reader
  *

  (import scheme chicken
          centring.bootstrap)

  (use coops coops-primitive-objects)

  ;;;;

  (def (inc n) (+ n 1))

  (defenum Option
    (Some val)
    (None))

  (def None (Option/None))
  (def Some Option/Some)

  (define-method (unwrap (some <Option/Some>))
    (slot-value some 'val))
  
;;   (define-method (fmap (f <procedure>) (opt <Option>))
;;     (match opt
;;       (? Option/Some?) (Some (f (slot-value opt 'val)))
;;       (? Option/None?) None))

  (defenum Result
    (Ok val)
    (Err err))

  (def Ok Result/Ok)
  (def Err Result/Err)

  (define-method (unwrap (ok <Result/Ok>))
    (slot-value ok 'val))

  (define-generic (count coll))

  (define-method (count (s <string>))
    (string-length s))

  (define-method (nth (s <string>) i)
    (string-ref s i))

  (define-class <StringSeq> ()
    (str pos))

  (define-method (seq (s <string>))
    (make <StringSeq> 'str s 'pos 0))

  (define (eof? sseq)
    (let ((str (slot-value sseq 'str))
          (pos (slot-value sseq 'pos)))
      (>= pos (count str))))

  (define-method (first (sseq <StringSeq>))
    (if (eof? sseq)
      None
      (Some (nth (slot-value sseq 'str) (slot-value sseq 'pos)))))

  (define-method (rest (sseq <StringSeq>))
    (if (eof? sseq)
      sseq
      (make <StringSeq>
         'str (slot-value sseq 'str)
         'pos (inc (slot-value sseq 'pos)))))

  ;;;;

  ;; Parser((StringSeq) -> (Result<Option<Any>, ReadError>, StringSeq))
  (define-class <Parser> ()
    (pf))

  (def (parse p sseq)
    ((slot-value p 'pf) sseq))

  ;; (Parser, (Option<Any> -> Parser)) -> Parser
  (define-method (mbind (p <Parser>) f)
    (make <Parser>
      'pf (lambda (sseq)
            (receive (res cs) (parse p sseq)
              (match res
                (? Result/Ok?)  (parse (f (unwrap res)) cs)
                (? Result/Err?) (values res cs)))))))

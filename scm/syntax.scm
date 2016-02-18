(use coops coops-primitive-objects
     (srfi 69)
     (only matchable match-lambda*)
     (only anaphora aif))

;;;; Data Representation
;;;; ===========================================================================

(define-class <Value> ())

;;; Types

(define-class <AbstractType> (<Value>)
  (name
   supertype))

(define-class <RecordType> (<Value>)
  (name
   supertype
   field-names))

(define-class <SingletonType> (<Value>)
  (name
   supertype))

;;; Instances of User-defined Types

(define-class <Record> (<Value>)
  (type
   field-vals))

(define-class <Singleton> (<Value>)
  (type))

;;; Immediate Types

(define-class <Int> (<Value>)
  (val))

(define-class <Bool> (<Value>)
  (val))

;;; Callables

(define-class <Closure> (<Value>)
  (name
   formal-types
   formal-names
   body
   env))

(define-class <NativeFn> (<Value>)
  (name
   formal-types
   code))

(define-class <MultiFn> (<Value>)
  (name
   methods))

;;;; Tools for Working with Centring Data
;;;; ===========================================================================

(define (make-record rectype . field-vals)
  (make <Record>
    'type rectype
    'field-vals (list->vector field-vals)))

(define singleton-instances (make-hash-table))

(define (make-singleton singtype)
  (aif (hash-table-ref/default singleton-instances singtype #f)
    it
    (let ((instance (make <Singleton> 'type singtype)))
      (hash-table-set! singleton-instances singtype instance)
      instance)))

(define-generic (make-value type))

(define-method (make-value (type <RecordType>) . field-vals)
  (apply make-record type field-vals))

(define-method (make-value (type <SingletonType>))
  (make-singleton type))

;;;; Actual Types
;;;; ===========================================================================

(define Any (make <AbstractType>))

(define Symbol (make <RecordType>
                 'supertype Any
                 'field-names #("module" "name")))

(define ctr-symbol
  (match-lambda*
    ((name) (make-record Symbol "" name))
    ((module name) (make-record Symbol module name))))

(set! (slot-value Any 'name) (ctr-symbol "centring.core" "Any"))
(set! (slot-value Any 'supertype) Any)

(set! (slot-value Symbol 'name) (ctr-symbol "centring.core" "Symbol"))

(define List (make <AbstractType>
               'name (ctr-symbol "centring.core" "List")
               'supertype Any))

(define List.Pair (make <RecordType>
                    'name (ctr-symbol "centring.core" "List.Pair")
                    'supertype List
                    'field-names #("left" "right")))

(define List.Empty (make <SingletonType>
                     'name (ctr-symbol "centring.core" "List.Empty")
                     'supertype List))

;;;; Main
;;;; ===========================================================================

(define (main _)
  (write (eq? (make-value List.Empty) (make-value List.Empty))))


;; (include "bootstrap.scm")
;; 
;; (module centring.reader
;;   *
;; 
;;   (import scheme chicken
;;           centring.bootstrap)
;; 
;;   (use coops coops-primitive-objects)
;; 
;;   ;;;;
;; 
;;   (def (inc n) (+ n 1))
;; 
;;   (defenum Option
;;     (Some val)
;;     (None))
;; 
;;   (def None (Option/None))
;;   (def Some Option/Some)
;; 
;;   (define-method (unwrap (some <Option/Some>))
;;     (slot-value some 'val))
;;   
;; ;;   (define-method (fmap (f <procedure>) (opt <Option>))
;; ;;     (match opt
;; ;;       (? Option/Some?) (Some (f (slot-value opt 'val)))
;; ;;       (? Option/None?) None))
;; 
;;   (defenum Result
;;     (Ok val)
;;     (Err err))
;; 
;;   (def Ok Result/Ok)
;;   (def Err Result/Err)
;; 
;;   (define-method (unwrap (ok <Result/Ok>))
;;     (slot-value ok 'val))
;; 
;;   (define-generic (count coll))
;; 
;;   (define-method (count (s <string>))
;;     (string-length s))
;; 
;;   (define-method (nth (s <string>) i)
;;     (string-ref s i))
;; 
;;   (define-class <StringSeq> ()
;;     (str pos))
;; 
;;   (define-method (seq (s <string>))
;;     (make <StringSeq> 'str s 'pos 0))
;; 
;;   (define (eof? sseq)
;;     (let ((str (slot-value sseq 'str))
;;           (pos (slot-value sseq 'pos)))
;;       (>= pos (count str))))
;; 
;;   (define-method (first (sseq <StringSeq>))
;;     (if (eof? sseq)
;;       None
;;       (Some (nth (slot-value sseq 'str) (slot-value sseq 'pos)))))
;; 
;;   (define-method (rest (sseq <StringSeq>))
;;     (if (eof? sseq)
;;       sseq
;;       (make <StringSeq>
;;          'str (slot-value sseq 'str)
;;          'pos (inc (slot-value sseq 'pos)))))
;; 
;;   ;;;;
;; 
;;   ;; Parser((StringSeq) -> (Result<Option<Any>, ReadError>, StringSeq))
;;   (define-class <Parser> ()
;;     (pf))
;; 
;;   (def (parse p sseq)
;;     ((slot-value p 'pf) sseq))
;; 
;;   ;; (Parser, (Option<Any> -> Parser)) -> Parser
;;   (define-method (mbind (p <Parser>) f)
;;     (make <Parser>
;;       'pf (lambda (sseq)
;;             (receive (res cs) (parse p sseq)
;;               (match res
;;                 (? Result/Ok?)  (parse (f (unwrap res)) cs)
;;                 (? Result/Err?) (values res cs)))))))

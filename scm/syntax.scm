(use coops coops-primitive-objects
     (srfi 69)
     (only matchable match match-let match-lambda*)
     (only anaphora aif acond)
     (only miscmacros define-syntax-rule)
     (only extras fprintf sprintf))

;;;; Utils
;;;; ===========================================================================

(define peek car)
(define push cons)

(define keyword->symbol (o string->symbol keyword->string))
(define symbol->keyword (o string->keyword symbol->string))

(define (map-indexed f ls)
  (define (mixed ls i)
    (match ls
      ('() '())
      (`(,v . ,vs) (cons (f i v) (mixed vs (+ i 1))))))
  (mixed ls 0))

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
    'field-vals field-vals))

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

(define (get-field rec fieldname)
  (let ((type (slot-value rec 'type)))
    (let recur ((names (slot-value type 'field-names))
                (vals (slot-value rec 'field-vals)))
      (cond
        ((null? names) (error "no such field" fieldname))
        ((eq? (car names) fieldname) (car vals))
        (else (recur (cdr names) (cdr vals)))))))

(define-syntax-rule (native-fn formals body ...)
    (make <NativeFn>
      'code (lambda formals
              body ...)))

;;;; Actual Types
;;;; ===========================================================================

(define Any (make <AbstractType>
              'name 'Any))
(set! (slot-value Any 'supertype) Any)

(define Symbol (make <RecordType>
                 'name 'Symbol
                 'supertype Any
                 'field-names '(module name)))

(define List (make <AbstractType>
               'name 'List
               'supertype Any))

(define List.Pair (make <RecordType>
                    'name 'List.Pair
                    'supertype List
                    'field-names '(left right)))

(define List.Empty (make <SingletonType>
                     'name 'List.Empty
                     'supertype List))

(define None (make <SingletonType>
               'name 'None
               'supertype Any))

;;;; More Data Tools
;;;; ===========================================================================

(define-generic (analyze expr))

(define-method (analyze (expr <fixnum>))
  (make <Int> 'val expr))

(define-method (analyze (expr <boolean>))
  (make <Bool> 'val expr))

(define-method (analyze (expr <null>))
  (make-value List.Empty))

(define-method (analyze (expr <pair>))
  (make-value List.Pair (analyze (car expr)) (analyze (cdr expr))))

(define (list->List ls)
  (match ls
    ('() (make-value List.Empty))
    (`(,head . ,tail) (make-value List.Pair head (list->List tail)))))

;;;; Interpretation
;;;; ===========================================================================

(define-class <Interpreter> ()
  ((envstack (list (make <Environment>)))
   (module-registry (make-hash-table))))

(define (current-env itp)
  (peek (slot-value itp 'envstack)))

(define-generic (interpret itp expr))
(define-generic (interpret-stmts itp stmts))
(define-generic (interpret-call itp callee args))

(define-method (interpret (itp <Interpreter>) (expr <fixnum>))
  (analyze expr))

(define-method (interpret (itp <Interpreter>) (expr <boolean>))
  (analyze expr))

(define-method (interpret (itp <Interpreter>) (expr <symbol>))
  (lookup itp (current-env itp) expr))

(define-method (interpret (itp <Interpreter>) (expr <pair>))
  (match (ctr-expand expr)
    (`(quote ,e) (analyze e))
    (`(def ,name ,value) (interpret-def itp name value))
    (`(do . ,stmts) (interpret-stmts itp stmts))

    (`(|.| ,rec ,fieldname) (get-field (interpret itp rec) fieldname))
    (`(record-type ,name ,fields) (make <RecordType>
                                    'name name
                                    'supertype Any
                                    'field-names fields))

    (`(fn ,name ,formals ,body) (interpret-fn itp name formals body))
    (`(fn ,formals ,body) (interpret-fn itp (gensym 'fn) formals body))
    (`(,callee . ,args) (interpret-call itp
                          (interpret itp callee)
                          (map (cute interpret itp <>) args)))))

(define (interpret-def itp name value)
  (let ((env (current-env itp)))
    (extend env name (interpret itp value))
    (make-value None)))

(define-method (interpret-stmts (itp <Interpreter>) (stmts <null>))
  (make-value None))

(define-method (interpret-stmts (itp <Interpreter>) (stmts <pair>))
  (match stmts
    (`(,e) (interpret itp e))
    (`(,e . ,es) (interpret itp e) (interpret-stmts itp es))))

(define (analyze-formals itp formals)
  (match formals
    ('()
     (values '() '()))
    (`(& ,name ,(and (? keyword?) type))
     (values name (interpret itp (keyword->symbol type))))
    (`(& ,name)
     (values name Any))
    (`(,name ,(and (? keyword?) type)  . ,rfs)
     (receive (names types) (analyze-formals itp rfs)
       (values (cons name names)
               (cons (interpret itp (keyword->symbol type)) types))))
    (`(,name . ,rfs)
     (receive (names types) (analyze-formals itp rfs)
       (values (cons name names) (cons Any types))))))

(define (interpret-fn itp name formals body)
  (receive (formal-names formal-types) (analyze-formals itp formals)
    (make <Closure>
      'name name
      'formal-names formal-names
      'formal-types formal-types
      'body body
      'env (current-env itp))))

(define-method (interpret-call (itp <Interpreter>) (fn <Closure>) args)
  (let ((formals (slot-value fn 'formal-names))
        (body (slot-value fn 'body))
        (env (make <Environment>
                'bindings (make-hash-table)
                'parent (slot-value fn 'env))))
    (bind-args formals args env)
    (interpret (make <Interpreter>
                  'envstack (push env (slot-value itp 'envstack))
                  'module-registry (slot-value itp 'module-registry))
               body)))

(define-method (interpret-call (itp <Interpreter>) (t <SingletonType>) args)
  (make-singleton t))

(define-method (interpret-call (itp <Interpreter>) (t <RecordType>) args)
  (apply make-record t args))

(define-method (interpret-call (itp <Interpreter>) (fn <NativeFn>) args)
  ((slot-value fn 'code) itp args))

;; Args should already be evaluated and env built:
(define-generic (bind-args formals args env))

(define-method (bind-args (formals <null>) (args <null>) env)
  env)

(define-method (bind-args (formals <pair>) (args <pair>) env)
  (extend env (car formals) (car args))
  (bind-args (cdr formals) (cdr args) env))

(define-method (bind-args (formals <pair>) (args <null>) env)
  (error "too few arguments!"))

(define-method (bind-args (formals <null>) (args #t) env)
  (error "too many arguments!"))

(define-method (bind-args (formals <symbol>) (args #t) env)
  (extend env formals (list->List args)))

(define (prepend-dot sym)
  (string->symbol (sprintf ".~S" sym)))

(define (ctr-expand expr)
  (match expr
    (`(def (,name . ,formals) . ,body)
     `(def ,name (fn ,name ,formals ,@body)))
    (`(defrecord (,name . ,fields))
     `(do
        (def ,name (record-type ,name ,fields))
        ,@(map-indexed
            (lambda (i fieldname)
              `(def (,(prepend-dot fieldname) rec ,(symbol->keyword name))
                 (nth-field rec ,i)))
            fields)))
    (`(fn ,(and (? symbol?) name) ,formals . ,body)
     `(fn ,name ,formals (do ,@body)))
    (`(fn ,(and (? list?) formals) . ,body)
     `(fn ,formals (do ,@body)))
    (_ expr)))

;;;; Environments
;;;; ===========================================================================

(define-class <Environment> ()
  ((bindings (make-hash-table))
   (parent #f)))

(define-generic (lookup itp env name))
(define-generic (extend env name val))

(define-method (lookup (itp <Interpreter>) (env <Environment>) (sym <symbol>))
  (acond
    ((hash-table-ref/default (slot-value env 'bindings) sym #f)
      it)
    ((slot-value env 'parent) (lookup itp it sym))
    (else (error "can't reference unbound variable" sym))))

(define-method (extend (env <Environment>) name val)
  (hash-table-set! (slot-value env 'bindings) name val)
  env)

;;;; Printing
;;;; ===========================================================================

(define-method (print-object (rec <Record>) port)
  (fprintf port "#.~S" (cons (slot-value (slot-value rec 'type) 'name)
                             (slot-value rec 'field-vals))))

(define-method (print-object (s <Singleton>) port)
  (fprintf port "#.(~S)" (slot-value (slot-value s 'type) 'name)))

(define-method (print-object (n <Int>) port)
  (write (slot-value n 'val) port))

(define-method (print-object (b <Bool>) port)
  (write (slot-value b 'val) port))

;;;; Core
;;;; ===========================================================================

(define centring.core
  (make <Environment>
    'bindings
    (alist->hash-table
      `((nth-field . ,(native-fn (itp args)
                        (match-let (((rec n) args))
                          (list-ref (slot-value rec 'field-vals)
                                    (slot-value n 'val)))))))))

;;;; Main
;;;; ===========================================================================

(define (main _)
  (write (get-field (make-value List.Pair "foo" (make-value List.Empty))
                    'left)))


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

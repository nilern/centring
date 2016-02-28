(use coops coops-primitive-objects
     (srfi 69)
     (only (srfi 1) proper-list? count)
     (only data-structures sort)
     (only matchable match match-let match-lambda*)
     (only anaphora aif acond awhen)
     (only miscmacros define-syntax-rule let/cc)
     (only extras fprintf sprintf read-file)
     (only linenoise linenoise history-add))

(declare (block) (local)
         (inline) (specialize))
(keyword-style #:prefix)

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

(define (repeat n v)
  (if (zero? n)
    '()
    (cons v (repeat (sub1 n) v))))

(define (take-while pred? ls)
  (cond
   ((null? ls) ls)
   ((pred? (car ls)) (cons (car ls) (take-while pred? (cdr ls))))
   (else '())))

;; The length of a list ignoring the '()/anything else at the end:
(define (proper-length ls)
  (define (plen ls l)
    (if (pair? ls)
      (plen (cdr ls) (add1 l))
      l))
  (plen ls 0))

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

(define-class <Bool> (<Value>)
  (val))

(define-class <Int> (<Value>)
  (val))

(define-class <Char> (<Value>)
  (val))

;;; Builtin Non-immediates

(define-class <Tuple> (<Value>)
  (val))

(define-class <Array> (<Value>)
  (val))

(define-class <String> (<Value>)
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
   (methods (make-hash-table))))

(define-class <Continuation> (<Value>)
  (cont))

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

(define-generic (ctr-type val))
(define-generic (supertype val))
(define-generic (isa? parent descendant))
;; TODO: Methods for the rest of <Value>

(define-method (ctr-type (rec <Record>)) (slot-value rec 'type))
(define-method (ctr-type (s <Singleton>)) (slot-value s 'type))

(define-method (supertype (rt <AbstractType>)) (slot-value rt 'supertype))
(define-method (supertype (rt <RecordType>)) (slot-value rt 'supertype))
(define-method (supertype (st <SingletonType>)) (slot-value st 'supertype))

(define-method (isa? (st1 <SingletonType>) (st2 <SingletonType>))
  (eq? st1 st2))

(define-method (isa? (st <SingletonType>) (rt <RecordType>))
  #f)

(define-method (isa? (rt <RecordType>) (st <SingletonType>))
  #f)

(define-method (isa? (rt1 <RecordType>) (rt2 <RecordType>))
  (eq? rt1 rt2))

(use-for-syntax (only matchable match-let))

(define-syntax native-fn
  (ir-macro-transformer
   (lambda (expr inject compare)
     (match-let (((_ (name itp formals formal-types) . body) expr))
       `(make <NativeFn>
          'name (quote ,name)
          'formal-types (list ,@formal-types)
          'code (lambda (,itp args)
                  (match-let ((,formals args))
                    ,@body)))))))

(define (add-method! fn meth)
  (hash-table-set! (slot-value fn 'methods)
                   (slot-value meth 'formal-types) meth))

;;;; Actual Types
;;;; ===========================================================================

(define Any (make <AbstractType>
              'name 'Any))
(set! (slot-value Any 'supertype) Any)

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

(define-method (analyze (expr <boolean>))
  (make <Bool> 'val expr))

(define-method (analyze (expr <fixnum>))
  (make <Int> 'val expr))

(define-method (analyze (expr <char>))
  (make <Char> 'val expr))

(define-method (analyze (expr <string>))
  (make <String> 'val expr))

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
(define-generic (interpret-branch itp cond then else))
(define-generic (interpret-call itp callee args)) ; TODO: check args

(define-method (interpret (itp <Interpreter>) (expr <boolean>))
  (analyze expr))

(define-method (interpret (itp <Interpreter>) (expr <fixnum>))
  (analyze expr))

(define-method (interpret (itp <Interpreter>) (expr <char>))
  (analyze expr))

(define-method (interpret (itp <Interpreter>) (expr <string>))
  (analyze expr))

(define-method (interpret (itp <Interpreter>) (expr <symbol>))
  (lookup itp (current-env itp) expr))

(define-method (interpret (itp <Interpreter>) (expr <pair>))
  (match (ctr-expand expr)
    (`(quote ,e) (analyze e))
    (`(def ,name ,value) (interpret-def itp name value))
    (`(do . ,stmts) (interpret-stmts itp stmts))
    (`(if ,cond ,then ,else) (interpret-branch
                              itp (interpret itp cond) then else))

    (`(|.| ,rec ,fieldname) (get-field (interpret itp rec) fieldname))
    (`(record-type ,name ,fields) (make <RecordType>
                                    'name name
                                    'supertype Any
                                    'field-names fields))
    (`(matches? ,pattern ,expr)
     (analyze (matches? itp pattern (interpret itp expr))))

    (`(fn ,name ,formals ,body) (interpret-fn itp name formals body))
    (`(fn ,formals ,body) (interpret-fn itp (gensym 'fn) formals body))

    (`(callcc ,callee) (let/cc ki
                         (let ((k (make <Continuation> 'cont ki)))
                           (interpret-call itp (interpret itp callee) `(,k)))))
    
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

(define-method (interpret-branch (itp <Interpreter>) (cond <Bool>) then else)
  (if (slot-value cond 'val)
    (interpret itp then)
    (interpret itp else)))

(define-method (interpret-branch (itp <Interpreter>) (cond #t) then else)
  (interpret itp then))
(define-method (interpret-stmts (itp <Interpreter>) (stmts <null>))
  (make-value None))

(define-method (interpret-stmts (itp <Interpreter>) (stmts <pair>))
  (match stmts
    (`(,e) (interpret itp e))
    (`(,e . ,es) (interpret itp e) (interpret-stmts itp es))))

(define-method (interpret-branch (itp <Interpreter>) (cond <Bool>) then else)
  (if (slot-value cond 'val)
    (interpret itp then)
    (interpret itp else)))

(define-method (interpret-branch (itp <Interpreter>) (cond #t) then else)
  (interpret itp then))

(define (matches? itp pattern v)
  (match pattern
    ((? symbol?) #t)
    (`(,stype) (isa? (interpret itp stype) (ctr-type v)))
    (`(,type . ,arg-pats)
     (and
       (isa? (interpret itp type) (ctr-type v))
       (= (count (cute matches? itp <> <>)
                 arg-pats (slot-value v 'field-vals))
          (length arg-pats))))))

(define (analyze-formals itp formals)
  (match formals
    ('()
     (values '() '()))
    (`(& ,(and (? symbol?) name) ,(and (? keyword?) type))
     (values name (interpret itp (keyword->symbol type))))
    (`(& ,(and (? symbol?) name))
     (values name Any))
    (`(& (= ,typename)) ; What use can this ever be? Why not, though.
     (values '_ `(= ,(interpret itp typename))))
    (`(,(and (? symbol?) name) ,(and (? keyword?) type)  . ,rfs)
     (receive (names types) (analyze-formals itp rfs)
       (values (cons name names)
               (cons (interpret itp (keyword->symbol type)) types))))
    (`(,(and (? symbol?) name) . ,rfs)
     (receive (names types) (analyze-formals itp rfs)
       (values (cons name names) (cons Any types))))
    (`((= ,typename) . ,rfs)
     (receive (names types) (analyze-formals itp rfs)
       (values (cons '_ names) (cons `(= ,(interpret itp typename)) types))))
    (_ (error "invalid formals" formals))))

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

(define-method (interpret-call (itp <Interpreter>) (k <Continuation>) args)
  (match args
    ('() ((slot-value k 'cont) (make-value None)))
    (`(,v) ((slot-value k 'cont) v))))

(define-method (interpret-call (itp <Interpreter>) (fn <MultiFn>) args)
  (let* ((methods (slot-value fn 'methods))
         (methvecs (map (lambda (ftps)
                          (receive (dd va?) (dispatch-distance ftps args #t)
                            (vector dd va? ftps)))
                        (hash-table-keys methods)))
         (matchvecs (remove (lambda (methvec) 
                                (negative? (vector-ref methvec 0)))
                            methvecs)))
    (cond
     ((null? matchvecs) ; TODO: Print the nearest matches like Julia:
      (error (sprintf "no matching method in ~S for args ~S"
                      (slot-value fn 'name) args)))
     ((null? (cdr matchvecs))
      (interpret-call ; TODO: use the unsafe version (when it appears)
       itp (hash-table-ref methods (vector-ref (car matchvecs) 2)) args))
     (else
      (let* ((sorted-matchvecs (sort matchvecs prefer-methvec))
             (best-methvec (car sorted-matchvecs)))
        (if (dispatch-equal? best-methvec (cadr sorted-matchvecs))
          (error
            (sprintf "ambiguous methods in ~S: ~S"
                     (slot-value fn 'name)
                     (take-while (cute dispatch-equal? best-methvec <>)
                                 sorted-matchvecs)))
          (interpret-call ; TODO: use the unsafe version (when it appears)
           itp (hash-table-ref methods (vector-ref best-methvec 2)) args)))))))

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

(define (match-body matchee-name succeed mlines)
  (match mlines
    (`((,pattern ,expr) . ,mlines)
     (let ((fail (gensym 'fail)))
       `((letcc ,fail
           (if-let (,pattern matchee-name)
             (,succeed ,expr)
             (,fail)))
         ,@(match-body matchee-name succeed mlines))))
    ('() '((None)))))

(define (ctr-expand-1 expr)
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
    (`(fn ,(and (? symbol?) name) ,formals ,body)
     expr)
    (`(fn ,(and (? symbol?) name) ,formals . ,body)
     `(fn ,name ,formals (do ,@body)))
    (`(fn ,(and (? list?) formals) ,body)
     expr)
    (`(fn ,(and (? list?) formals) . ,body)
     `(fn ,formals (do ,@body)))
    (`(let ,bindings . ,body)
     `((fn ,(map car bindings) ,@body)
       ,@(map cadr bindings)))
    (`(letcc ,k . ,body)
     `(callcc (fn (,k) ,@body)))
    (`(if-let (,pattern ,cexpr) ,then ,else)
     (let ((matchee (gensym 'matchee)))
       `(let ((,matchee ,cexpr))
          (if (matches? ,pattern ,matchee)
            (let ((,pattern ,matchee))
              ,then)
            ,else))))
    (`(match ,matchee . ,mlines)
     (let ((matchee-name (gensym 'matchee))
           (succeed (gensym 'succeed)))
       `(let ((,matchee-name ,matchee))
          (letcc ,succeed
            (do 
              ,@(match-body matchee-name succeed mlines))))))
    (_ expr)))

(define (ctr-expand expr)
  (let ((expansion (ctr-expand-1 expr)))
    (if (eq? expansion expr)
      expr
      (ctr-expand expansion))))

;;;; Dispatch
;;;; ===========================================================================

(define (type-distance F T)
  (define (tdist F T acc)
    (cond
     ((eq? T F) acc)
     ((eq? T Any) -1)
     (else (tdist F (supertype T) (add1 acc)))))
  (tdist F T 0))

(define (arg-distance ftp arg)
  (match ftp
    (`(= ,F) (if (eq? arg F) 0 -1))
    (F (type-distance F (ctr-type arg)))))

(define (combine-distances pdd sdd)
  (if (eq? (negative? pdd) (negative? sdd))
    (+ pdd sdd)
    (min pdd sdd))) ; gets the negative one

;; This should be correct and fast. Maybe it is.
(define (dispatch-distance ftps args short-circuit?)
  (define (ddist ftps args pdd vararg?)
    (cond
     ((null? args) ; Out of args, let's see about ftps:
      (cond
       ((null? ftps) ; Lengths matched, we are done:
        (values pdd vararg?))
       ((pair? ftps) ; Too few args:
        (if short-circuit?
          (values (combine-distances pdd -1) vararg?)
          (values (combine-distances pdd (- (proper-length ftps)))
                  (or vararg? (not (proper-list? ftps))))))
       (else ; Vararg. Remember that and we are done:
        (values pdd #t))))
     ((null? ftps) ; Too many args.
      (values (combine-distances pdd (- (length args))) vararg?))
     ((pair? ftps) ; Both ftps and args left. recurse:
      (if (and short-circuit? (negative? pdd))
        (values pdd vararg?)
        (let ((adist (arg-distance (car ftps) (car args))))
          (ddist (cdr ftps) (cdr args) (combine-distances pdd adist) vararg?))))
     (else ; Vararg. Expand it and remember there was one:
      (ddist (repeat (length args) ftps) args pdd #t))))
  (ddist ftps args 0 #f))

(define (prefer-methvec mv1 mv2)
  (match-let ((#(dd1 va1? _) mv1)
              (#(dd2 va2? _) mv2))
    (if (= dd1 dd2)
      (and (not va1?) va2?)
      (< dd1 dd2))))

(define (dispatch-equal? mv1 mv2)
  (match-let ((#(dd1 va1? _) mv1)
              (#(dd2 va2? _) mv2))
    (and (= dd1 dd2) (eq? va1? va2?))))

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
  (let ((bindings (slot-value env 'bindings)))
    (let/cc return
      (hash-table-set! bindings name
        (aif (hash-table-ref/default bindings name #f)
          (let ((T (class-of it)))
            (cond
             ((eq? T <MultiFn>)
              (add-method! it val)
              (return #f))
             ((or (eq? T <Closure>) (eq? T <NativeFn>))
              (let ((fn (make <MultiFn> 'name (slot-value it 'name))))
                (add-method! fn it)
                (add-method! fn val)
                fn))
             (else
              val)))
          val))))
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
      `((nth-field . ,(native-fn (nth-field itp (rec n) (Any Any))
                        (list-ref (slot-value rec 'field-vals)
                                  (slot-value n 'val))))
        (Any . ,Any)
        (List . ,List)
        (List.Pair . ,List.Pair)
        (List.Empty . ,List.Empty)))))

;;;; Reader Modifications
;;;; ===========================================================================

(define ((read-ctor ctor-sym end-char) port)
  (let loop ((c (peek-char port)) (exprs '()))
    (cond
     ((eof-object? c) (error "EOF reached while parsing #(...)!"))
     ((char=? c end-char)
      (read-char port)
      `(,ctor-sym ,@(reverse exprs)))
     ((char-whitespace? c)
      (read-char port)
      (loop (peek-char port) exprs))
     (else
      (let ((expr (read port)))
        (loop (peek-char port) (cons expr exprs)))))))

(set-read-syntax! #\[ (read-ctor 'Vector #\]))
(set-read-syntax! #\{ (read-ctor 'HashMap #\}))

(set-sharp-read-syntax! #\( (read-ctor 'Tuple #\)))
(set-sharp-read-syntax! #\[ (read-ctor 'Array #\]))
(set-sharp-read-syntax! #\{ (read-ctor 'Set #\}))

(set-read-syntax! #\\
  (lambda (port)
    (let ((c (read-char port)))
      (if (eof-object? c)
        (error "expected character, got EOF.")
        c))))

;;;; Main
;;;; ===========================================================================

(define (main arglist)
  (if (null? arglist)
    (let ((itp (make <Interpreter> 'envstack (list centring.core))))
      (let recur ()
        (awhen (linenoise "ctr> ")
          (history-add it)
          (printf "~S~N" (interpret itp (with-input-from-string it read)))
          (recur))))
    (let ((expr (match arglist
                  (`("-e" ,estr) (with-input-from-string estr read))
                  (`(,filename) `(do ,@(read-file filename)))
                  (_ (error "invalid arguments" arglist) (exit 1)))))
      (printf "~S~N"
              (interpret
                (make <Interpreter>
                  'envstack (list centring.core))
                expr))))
  (exit 0))

(main (command-line-arguments))

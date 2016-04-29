(module centring.vm
  *

  (import scheme chicken)
  (use (only (srfi 69) hash-table-ref)
       (only matchable match)
       (only clojurian-syntax ->)

       array
       (prefix centring.cps cps:))
  
  ;;;; Utils

  ;;;; Value

  (define-record gsymbol
    module
    name)
  
  ;;;; VM

  (define-record fiber
    stack         ; ::array<value>
    sp            ; ::fixnum
    modules       ; ::hashtable<symbol, hashtable<symbol, value>>

    clovers       ; ::vector<value> U #f
    consts        ; ::vector<value> U #f
    global-names) ; ::vector<gsymbol> U #f

  (define (resolve fiber gsym)
    (-> (fiber-modules fiber)
        (hash-table-ref (gsymbol-module gsym))
        (hash-table-ref (gsymbol-name gsym))))

  (define (local-ref fiber i)
    (array-ref (fiber-stack fiber) (+ (fiber-sp fiber) i)))

  (define (push-local! fiber v)
    (array-push! (fiber-stack fiber) v))

  (define (global-ref fiber i)
    (resolve fiber (vector-ref (fiber-global-names fiber) i)))

  (define (clover-ref fiber i)
    (vector-ref (fiber-clovers fiber) i))

  (define (const-ref fiber i)
    (vector-ref (fiber-consts fiber) i))

  ;;;; Eval CPS

  (define (eval-cps fiber cexp)
    (match cexp
      (($ cps:If cond then else)
       (if (eval-cps fiber cond)
         (eval-cps fiber then)
         (eval-cps fiber else)))
      (($ cps:Primop op args _ conts) (eval-cps-primop fiber op args conts))
      (($ cps:Local i)     (local-ref fiber i))
      (($ cps:Global name) (resolve fiber name))
      (($ cps:Clover i)    (clover-ref fiber i))
      (($ cps:Const val)   val)))

  (define (eval-cps-primop fiber op args conts)
    (case op
      ((halt) (eval-cps fiber (car args)))
      ((add)
       (push-local! fiber (+ (eval-cps fiber (car args))
                             (eval-cps fiber (cadr args))))
       (eval-cps fiber (car conts))))))
       
        

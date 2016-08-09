(library (ctr cek)
  (export interpret)
  (import (rnrs (6))

          (only (util) defrecord inc dec)

          (only (ctr util) ctr-error)
          (only (ctr env) make-env)
          (ctr ast))

  ;;;; Continuations

  (defrecord (PrimopCont op vals asts index conts env cont))
  (defrecord (DoCont stmts index env cont))
  (defrecord (NsCont ns cont))
  (defrecord (HaltCont))

  ;;;; Machine
  (define (run ctrl env k)
    (cond
     ((Do? ctrl)
      (let ((stmts (Do-stmts ctrl)))
        ;; MAYBE: might need to deal with situation where stmts = #()
        (run (vector-ref stmts 0) env (make-DoCont stmts 0 env k))))
     ((Const? ctrl)
      (continue ctrl k))
     (else
      (ctr-error "unable to interpret" ctrl))))

  (define (continue ctrl k)
    (cond
     ((DoCont? k)
      (let ((stmts (DoCont-stmts k))
            (i (DoCont-index k))
            (env (DoCont-env k))
            (k* (DoCont-cont k)))
        ;; MAYBE: might need to deal with situation where stmts = #()
        (if (= i (dec (vector-length stmts)))
          (run ctrl env k*)
          (let ((i* (inc i)))
            (run (vector-ref stmts i*) env (make-DoCont stmts i* env k*))))))
     ((HaltCont? k)
      (Const-val ctrl))
     (else
      (ctr-error "unrecognized continuation" k))))

  (define (interpret ast)
    (run ast (make-env) (make-HaltCont))))

  ;; ;;;; Machine

  ;; ;; TODO: break this up and let alpha conversion do its work:
  ;; (define (interpret ctrl)
  ;;   (define (run ctrl env k)
  ;;     (match ctrl
  ;;       ;; When ctrl is complex, start from first subexpr
  ;;       ;; and build a continuation:
  ;;       (($ Primop op args conts)
  ;;        (run (vector-ref args 0)
  ;;             env
  ;;             (Primop-cont op
  ;;                          (make-vector (vector-length args))
  ;;                          args
  ;;                          0
  ;;                          conts env k)))
  ;;       (($ Do stmts)
  ;;        (run (vector-ref stmts 0) env (Do-cont stmts 0 env k)))

  ;;       ;; When down to a constant, need to examine continuation:
  ;;       (($ Const v)
  ;;        (match k
  ;;          (($ Primop-cont op vals args i conts env* k)
  ;;           (let ((i* (add1 i))
  ;;                 (vals* (doto (vector-copy vals) (vector-set! i v))))
  ;;             (if (= i* (vector-length args))
  ;;               ;; perform operation:
  ;;               ;; TODO: embed *Instr in AST to remove the hash-ref:
  ;;               (case op ; HACK
  ;;                 ((apply)
  ;;                  (match-let ((#(fn arg) vals*))
  ;;                    (match fn
  ;;                      (($ FnClosure formal _ _ _)
  ;;                       (run (fn-body fn) (make-env (FnClosure-formal fn) arg) k))
  ;;                      (($ NativeFn _ fn ret)
  ;;                       ;; TODO: check tupleness, signal errors:
  ;;                       ;; TODO: do conversions properly:
  ;;                       ;; TODO: optimize
  ;;                       (define (ctr->scm v)
  ;;                         (match v
  ;;                           ((? literal?) v)
  ;;                           ;; this is only right for FFI:
  ;;                           (($ BytesInstance _ bytes) bytes)
  ;;                           (_ (error "unable to convert" v))))
  ;;                       (define (scm->ctr v)
  ;;                         (match v
  ;;                           ((? literal?) v)
  ;;                           ((? string?)
  ;;                            (vector
  ;;                             (ns-lookup (ns-ref 'ctr.lang) #f 'String)
  ;;                             (string->utf8 v)))
  ;;                           (_ (error "unable to convert" v))))
  ;;                       (run (Const (->> arg
  ;;                                        vector->list
  ;;                                        pop
  ;;                                        (map ctr->scm)
  ;;                                        ((flip append) `(return: ,ret))
  ;;                                        (apply fn)
  ;;                                        scm->ctr))
  ;;                            env* k))
  ;;                      (($ Continuation k)
  ;;                       (run (Const arg) #f k))
  ;;                      (_ ; TODO: optimize:
  ;;                       (run (Primop 'apply
  ;;                                    (vector
  ;;                                     (Symbol 'ctr.lang 'apply)
  ;;                                     (Primop 'rec
  ;;                                             (vector
  ;;                                              (Symbol 'ctr.lang 'Tuple)
  ;;                                              (Const fn)
  ;;                                              (Const arg))
  ;;                                             #f))
  ;;                                    #f)
  ;;                            env* k)))))
  ;;                 ((apply-cc)
  ;;                  (let ((fn (vector-ref vals* 0)))
  ;;                    (run (fn-body fn)
  ;;                         (make-env (FnClosure-formal fn) (Continuation k))
  ;;                         k)))
  ;;                 ((defined?)
  ;;                  (let ((defined? 
  ;;                          (try
  ;;                           (match-let ((#(($ Symbol ns name)) vals*))
  ;;                             (env-lookup env* ns name)
  ;;                             #t)
  ;;                           (catch _
  ;;                             #f))))
  ;;                    (run (Const defined?) #f k)))
  ;;                 (else
  ;;                  (match (hash-table-ref primops op)
  ;;                    (($ ExprOp impl)
  ;;                     (run (Const (impl vals*)) #f k))
  ;;                    (($ StmtOp impl)
  ;;                     (impl vals*)
  ;;                     (let ((tup (vector (ns-lookup (ns-ref 'ctr.lang)
  ;;                                                   #f 'Tuple))))
  ;;                       (run (Const tup) #f k)))
  ;;                    (($ CtrlOp impl)
  ;;                     (run (impl conts vals*) env* k)))))
  ;;               ;; evaluate next argument:
  ;;               (run (vector-ref args i*)
  ;;                    env*
  ;;                    (Primop-cont op vals* args i* conts env* k)))))
  ;;          (($ Do-cont stmts i env* k)
  ;;           (if (= i (sub1 (vector-length stmts)))
  ;;             ;; last value gets passed to the continuation of the Do:
  ;;             (run ctrl env* k)
  ;;             ;; throw value away and evaluate the next statement:
  ;;             (let ((i* (add1 i)))
  ;;               (run (vector-ref stmts i*) env* (Do-cont stmts i* env* k)))))
  ;;          (($ NsCont ns* k)
  ;;           (current-ns ns*)
  ;;           (run ctrl env k))
  ;;          (($ Halt-cont)
  ;;           v)
  ;;          (_ (error "unrecognized continuation" k))))

  ;;       ;; For Fns, build a closure:
  ;;       (($ Fn formal cases _)
  ;;        (run (Const (make-fn formal cases env)) env k))

  ;;       ;; For Symbols, look up the corresponding value:
  ;;       (($ Symbol ns name)
  ;;        (run (Const (env-lookup env ns name)) env k))

  ;;       ;; For Closures, restore the env and merge the current one in.
  ;;       ;; the current one should always be just {formal arg}.
  ;;       (($ Closure expr env* ns*)
  ;;        (let ((ns (current-ns)))
  ;;          (current-ns ns*)
  ;;          (run expr (env-merge env* env) (NsCont ns k))))

  ;;       (_ (error "unable to interpret" ctrl))))
  ;;   (run ctrl (make-env) (Halt-cont))))

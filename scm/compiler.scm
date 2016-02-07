(include "bootstrap.scm")
(import centring.bootstrap)

(use coops)

;;;; AST

(defenum ASTNode
  (Lit val)
  (Var sym)
  (Set lval rval)
  (Do statements)
  (If condition then else)
  (Call op args)
  (Fn formals body))

(defmethod (print-object node :ASTNode port)
  (write 
    (match node
      (? ASTNode/Lit?)  (slot-value node 'val)
      (? ASTNode/Var?)  (slot-value node 'sym)
      (? ASTNode/Set?)  `(set! ,(slot-value node 'lval) ,(slot-value node 'rval))
      (? ASTNode/Do?)   `(do ,@(slot-value node 'statements))
      (? ASTNode/If?)   `(if ,(slot-value node 'condition)
                             ,(slot-value node 'then)
                             ,(slot-value node 'else))
      (? ASTNode/Call?) `(,(slot-value node 'op) ,@(slot-value node 'args))
      (? ASTNode/Fn?)   `(fn ,(slot-value node 'formals)
                             ,(slot-value node 'body)))
    port))

;;;; Parse

;; Chicken's (read) is OK for now.

;;;; Macroexpand

;; Something like ir-macro-transformer or Julia macros? Need to study hygiene.
;; Not essential for trivial examples.

;;;; Analyze

(def (analyze sexpr)
  (match sexpr
    (or (? integer?)
        (? boolean?))       (ASTNode/Lit sexpr)
    (? symbol?)             (ASTNode/Var sexpr)
    `(set! ,lval ,rval)     (ASTNode/Set (analyze lval) (analyze rval))
    `(do . ,stmts)          (ASTNode/Do (map analyze stmts))
    `(if ,cond ,then ,else) (ASTNode/If (analyze cond)
                                        (analyze then)
                                        (analyze else))
    `(fn ,args ,body)       (ASTNode/Fn (map analyze args) (analyze body))
    `(fn ,args . ,body)     (analyze `(fn ,args (do ,@body)))
    `(,op . ,args)          (ASTNode/Call (analyze op) (map analyze args))))

;;;; CPS

;;;; Closure Conversion

;; Flat closures.

;;;; Optimization

;; No need to hurry with these.

;;;; Code Generation

;; C or LLVM IR?

;;;; Main

(writeln (analyze '((fn (n)
                      (if (< n 1)
                        1
                        (* n (recur (- n 1)))))
                    5)))

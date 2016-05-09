Centring, a Modern Lisp
=======================

With inspiration from Clojure, Julia, Scheme, Haskell, Rust etc.

**Under heavy development, not usable.**

I'm mostly making this to learn about the implementation of programming languages,
with a focus on continuation-passing style, virtual machines and dispatch.

Hopefully it will eventually also be useful for implementing something else.

Example
-------

```clojure
(namespace math-expr)

;; Enums / tagged unions:
(defenum MathExpr
  (Sum left right)
  (Difference left right)
  (Product left right)
  (Quotient left right)
  (Literal val))

;; Multimethods:
(def (+ l :MathExpr r :MathExpr)
  (MathExpr.Sum l r))

(def (- l :MathExpr r :MathExpr)
  (MathExpr.Difference l r))
  
(def (* l :MathExpr r :MathExpr)
  (MathExpr.Product l r))
  
(def (/ l :MathExpr r :MathExpr)
  (MathExpr.Quotient l r))

;; Pattern matching:
(def (eval-math e :MathExpr)
  (match e
    ((MathExpr.Sum l r)        (+ (eval-math l) (eval-math r)))
    ((MathExpr.Difference l r) (- (eval-math l) (eval-math r)))
    ((MathExpr.Product l r)    (* (eval-math l) (eval-math r)))
    ((MathExpr.Quotient l r)   (/ (eval-math l) (eval-math r)))
    ((MathExpr.Literal val)    val)))

(let ((a (MathExpr.Literal 2))
      (b (MathExpr.Literal 3))
      (c (MathExpr.Literal 6))
      (d (MathExpr.Literal 2)))
  (eval-math (* (+ a b) (- c d))))

; => 20
```

Initial Implementation Strategy
-------------------------------

* Modify Chicken Scheme's reader and use that to parse S-expressions.
* Do some built-in 'macro expansion' in Scheme.
* Convert the S-expressions to a direct-style AST.
* Alpha-convert the AST and mark local variables as such.
* Convert the direct-style AST to a CPS representation.
* Do some optimizations to reduce the verbosity of the CPS representation.
* Closure-convert the CPS representation.
* Emit a kind of threaded code.
* Run the threaded code.

Then it will be possible to

* Write the standard library.
* Write a self-hosting compiler that has a proper extensible reader and a
  hygienic macro system.

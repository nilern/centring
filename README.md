Centring, a Modern Lisp
=======================

With inspiration from Clojure, Julia, Scheme, Haskell, Rust etc.

*Everything is in a flux*.

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

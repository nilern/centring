Centring, a Lisp with Predicate Dispatch
========================================

**Under heavy development, not usable.**

Examples
--------

```
(defenum Option
  (Some val)
  (None))

(def (fmap f s) (: s Some)
  (Some (f (.val s))))

(def (fmap _ n) (: n None)
  n)

(def (mret t v) (= t Option)
  (Some v))

(def (mbind s f) (: s Some)
  (f (.val s)))

(def (mbind n _) (: n None)
  n)
```

```
(def factorial
  (fn
    ((n) (zero? n)
     (one n))
    ((n) (> n (zero n))
     (* n (factorial (dec n))))))
```


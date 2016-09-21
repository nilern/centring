Centring, a Lisp with Predicate Dispatch
========================================

**Under heavy development, not usable.**

Planned Features
----------------

* Predicate dispatch and pattern matching
* Hygienic macros (using syntax objects)
* Call-cc (and dynamic-wind)
* Powerful module system (ML-inspired)
* FFI to native code
* Static (whole-program) compilation to native code

Examples
--------

```
(defenum Option
  (Some val)
  (None))

(defmethods fmap
  (#(f s) (: s Some)
   (Some (f (.val s))))
  (#(f n) (: n None)
   n))

(defmethods mret
  (#(T v) (= T Option)
   (Some v)))

(defmethods mbind
  (#(s f) (: s Some)
   (f (.val s)))
  (#(n _) (: n None)
   n))
```

```
(def factorial
  (fn
    (#(n) (zero? n)
     (one n))
    (#(n) (> n (zero n))
     (* n (factorial (dec n))))))
```

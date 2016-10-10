Centring, a Lisp with Predicate Dispatch
========================================

**Under heavy development, not usable.**

Planned Language Features
-------------------------

* Predicate dispatch and pattern matching
* Hygienic macros (using syntax objects)
* Reader macros
* Continuations (maybe delimited, maybe not)
* Static module system (Racket, ML inspired)
* Green threads (possibly CSP/Actors based)
* FFI to native code
* Persistent data structures

### Things to Avoid

* Encouraging mutability (immutable fields are the default, persistent data
  structures are the ones with predefined reader macros).
* Exceptions as default error handling (there will be exceptions, but only for
  truly exceptional things like arithmetic errors and OS signals).
* Having lists be easier to use than other data structures. And especially using
  cons cells for anything but lists.
* Casual use of `nil`/`#f` as some sort of error value (not as bad as NULL but
  still sloppy and tends to propagate in unexpected ways).
* Overly dynamic features (types are immutable, compilers aren't required to
  support `eval`).
* Having a C API (FFI makes new implementations easier to write).
* Prioritizing interoperability when making design choices (because, we already
  have Clojure).
* Using EDN instead of S-expressions (bootstrapping is kind of hard if you need
  to have persistent data structures just so you can `read`).

Planned Implementation Features
-------------------------------

* Decent performance
* Simple builds and deliverables

### Things to Avoid

* Having a JIT be the only reasonably fast implementation. (JITs are complex and
  can also be non-portable, unpredictable or not allowed on target systems).

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

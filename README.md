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

Interpreter Progress
--------------------

- Data
    - [ ] Records
        - [ ] Rest-fields
        - [ ] Setters
        - [ ] Callable
    - [ ] Bits types & byte arrays
    - [ ] Char
    - [ ] String
- CEK Machine
    - [ ] %apply-cc
    - [ ] %apply non-FnClosures
    - [x] Statements return #()
    - [ ] Multitude of primops
- Dispatch
    - [ ] Bug fixes
    - [ ] Optimizations
- [ ] FFI
- [ ] I/O
- Namespaces
    - [ ] Private vars
- `ctr.lang`
    - [ ] dynamic-wind
    - [ ] Exceptions (throw and catch)
- `ctr.core`
    - [ ] Green threads, CSP
    - [ ] Persistent data structures

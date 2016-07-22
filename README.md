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
        - [x] Rest-fields
        - [ ] Setters
        - [x] Callable
    - [ ] Bits types & byte arrays
    - [x] Char
    - [ ] String
    - [x] Continuation
- AST
    - [ ] give `fn`:s names (for self-recursion, execution traces)
- CEK Machine
    - [ ] $letrec
    - [x] %apply-cc
    - [x] %apply non-FnClosures
    - [x] Statements return #()
    - [ ] Multitude of primops
- Dispatch
    - [ ] Bug fixes
    - [ ] Optimizations
- [x] Errors
    - [ ] Execution traces
- [ ] FFI
- [ ] I/O
- Expand
    - [x] and-patterns
    - [x] `let`
    - [x] `match`
    - [ ] `defn`
- Namespaces
    - [ ] Private vars
- `ctr.lang`
    - [ ] Box
    - [ ] dynamic-wind
    - [ ] apply-cc (that maintains 'winders')
- `ctr.core`
    - [ ] Green threads, CSP
    - [ ] Persistent data structures
    - [ ] Ref types
    - [ ] Seq
    - [ ] Reducible
    - [ ] Transducers

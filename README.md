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
    - [ ] Bytes types
        - [x] Primops
        - [ ] Creation syntax
    - [x] Char
    - [x] String
    - [ ] Array
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
    - [ ] Overrides, disjointness of types
    - [ ] Bug fixes
    - [ ] Optimizations
    - [ ] Type classes
- [ ] Errors
    - [x] %err
    - [ ] Execution traces
- [ ] FFI
    - [x] %ffi-require
    - [x] %ffi-fn
    - [ ] Sensible conversion principles
- [ ] I/O
- Expand
    - [x] and-patterns
    - [x] `let`
    - [x] `match`
    - [x] `defn`
    - [x] ffi-require & ffi-fn
    - [ ] featureful ns and require form(s)
- Namespaces
    - [ ] Private vars
    - [ ] Store ffi-modules
    - [ ] `require` syntax (`as`, `refer`, `import`, `except`..?)
- `ctr.lang`
    - [ ] Box
    - [ ] dynamic-wind
    - [ ] apply-cc (that maintains 'winders')
- `ctr.core`
    - [ ] Green threads, CSP
    - [ ] Persistent data structures
    - [ ] Ref types
    - [ ] Seq
        - [x] seq, first, next, rest
    - [ ] Reducible
    - [ ] Transducers
- Fixes and refactoring
    - [ ] Avoid thowing 'Chicken Errors'
    - [ ] Conversions between ctr and scm
    - [ ] Reduce the number of Scheme files

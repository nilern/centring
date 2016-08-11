# Functions

A **function** such as

```clojure
    (def fact
      (fn
        (#(n) (zero? n)
         1)
        (#(n) (> n (zero n))
         (* n (fact (dec n))))))
```

* Takes one argument
* Consists of **partials**
    - Partials consist of a **condition** and a **body**
    - If the condition is true, the partial is **applicable**
    - A partial **overrides** another if its condition implies that of the other
* **Closes over** its definition environment
* Can be `merge`d or `merge!`'d

# Possible Implementing Objects

* Fn
    - partials[PartialFn]
* PartialFn
    - Proc + condition
* Closure
    - Proc + clovers[Any]
* GuardedProc
    - Proc + condition
* Proc
    - bytecode/ast
* NativeProc
    - code pointer

# Implementation

Transformations to do:

* DNF conversion
    - Canonicalize conditions
* Method fusion
    - Make dispatch explicit
* CPS conversion
    - Make evaluation order and stack explicit, enable call-cc
* Closure conversion
    - Make scope explicit
* Constant folding, dead code elimination and whatnot

Dependencies

* DNF conv -> Method fuse
* Method fuse -> CPS conv
* CPS conv -> Closure conv
* CPS conv -> Various optimizations

Would be nice if method fusion could be done at compile time since everything
else depends on it. For that to work we need to be able to statically determine
the partials of a function at all points.

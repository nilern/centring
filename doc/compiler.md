# Pseudo-namespaces

Pseudo-namespaces are specially handled by the compiler and do not exist at runtime.

## centring.sf

These are the special forms:

* if    (conditional branching)
* letfn (define functions (with arbitrary mutual recursion)
* def   (add a definition to the current module)
* quote (don't evaluate a symbol (`'(foo)` -> (cons 'foo ()) instead))

The special forms don't obey the normal evaluation rule.

## centring.intr

These are the intrinsics (correspond to VM instructions):

* const
* local
* clover

* add
* sub
* mul
* div

* brf
* halt

* fn
* call

The intrinsics can only be directly called so they are like 2nd-class functions.
Unlike the special forms they handle their arguments normally.

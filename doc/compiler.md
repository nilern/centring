# Pipeline

1. read (with modified Chicken reader)
2. analyze into AST
3. alpha-convert
4. CPS-convert
5. optimize (eta-contract, beta-contract, eliminate dead values)
6. closure-convert
7. emit bytecode
8. serialize bytecode

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

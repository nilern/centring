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

# IRs

## Trivial Expressions (common to AST & CPS)

1. Constants
2. Locals
3. Clovers
4. Globals

### Pseudocode

    (defenum TrivExp
      (Constant val ann)
      (Local name ann)
      (Clover index ann)
      (Global res-ns ns name ann))

## AST Representation

1. Nontrivial Expressions
    1. Do
    2. Fn
    3. Fix
    4. Call
2. Trivial Expressions

### Pseudocode

    (defenum AST
      (Do stmts ann)
      (Fn formals1 body ann)
      (Fix bindings body ann)
      (Call callee args ann)
      (... TrivExp))

### Passes

1. `specialize-identifiers`
    * Identify & alphatize Locals
    * Fill in `res-ns` for globals

## CPS Representation

1. Nontrivial Expressions
    1. Fn
        * Closure -> Procedure
        * -> Continuation
        * (Multimethod forms already expanded away)
    2. Fix (self- & mutually recursive definitions)
        * Fn:s
        * Closure constructions
    3. Call
        * Fn things
        * Primops (`centring.intr`)
2. Trivial Expressions

### Pseudocode

    (defenum CPS
      (Fn formals body ann)
      (Fix bindings body ann)
      (Call callee args conts ann)
      (... TrivExp))

## Symbol Table

Can store

1. Types

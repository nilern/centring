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

1. Constants (`1`, `#t`...)
2. Locals    (`a`, `b`...)
3. Clovers   (`(@ 0)`...)
4. Globals   (`@@/foo`, `centring.lang/Tuple`)

### Pseudocode

    (defenum TrivExp
      (Constant val ann)
      (Local name ann)
      (Clover index ann)
      (Global res-ns ns name ann))

## AST Representation

1. Nontrivial Expressions
    1. Do (`($do stmts ...)`)
    2. Fn (`($fn x (cond body) ...)`)
    3. Fix (`($fix ((x expr) ...) body)`)
    4. Call (`(%op arg ...)`)
2. Trivial Expressions

### Pseudocode

    (defenum AST
      (Do stmts ann)
      (Fn formals body ann)
      (Fix bindings body ann)
      (Call callee args ann)
      (... TrivExp))

### Passes

1. `specialize-identifiers`
    * Identify & alphatize Locals
    * Fill in `res-ns` for globals

## CPS Representation

1. Nontrivial Expressions
    1. Fn (`($fn (formal ...) (cond body) ...)`)
        * At first they have closure semantics
            - The cases in user-level functions have formals (v, k)
            - Continuations have just (v)
        * After closure conversion procedure semantics
            - Functions have (v, c, k)
            - Continuations (v, c) or (v) or anything really
        * What about when methods have been added and cases have different
          closures?
            - Not a problem with interpreters.
                * Except that the dispatch DAG gets really funky if clovers can
                  be referenced there!
            - Is (efficient) static compilation impossible?
    2. Fix (`($fix ((x expr) ...) body)`)
        * At first exprs are `$fn`:s, after closure conversion they are `%rec`:s
        * 'Linearized' away before the final stages
    3. Call (`(%op arg ... cont ...)`)
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

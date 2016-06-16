# Pipeline

1. read (with modified Chicken reader)
2. analyze into AST
3. alpha-convert
4. DNF-convert
5. CPS-convert
6. optimize (eta-contract, beta-contract, eliminate dead values)
7. closure-convert
8. emit bytecode
9. serialize bytecode

# Pseudo-namespaces

Pseudo-namespaces are specially handled by the compiler and do not exist at runtime.

## centring.sf

These are the special forms:

* fn
* letrec
* do
* quote

The special forms don't obey the normal evaluation rule.

## centring.intr

These are the intrinsics (correspond to VM instructions).

The intrinsics can only be directly called so they are like 2nd-class functions.
Unlike the special forms they handle their arguments normally.

# IRs

## Trivial Expressions (common to AST & CPS)

1. Constants (`1`, `#t`...)
2. Globals   (`@@/foo`, `centring.lang/Tuple`)
3. Locals    (`a`, `b`...)
4. Clovers   (`(@ 0)`...)

## AST Representation

1. Nontrivial Expressions
    1. Fn (`($fn x (cond body) ...)`)
    2. Primop (`(%op arg ...)`)
    3. Fix (`($fix ((x expr) ...) body)`)
    4. Do (`($do stmts ...)`)
2. Trivial Expressions

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

## Symbol Table

Can store

1. Types

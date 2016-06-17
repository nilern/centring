# Semantics

## Functions

* Every function takes one argument
    - Simplifies the implementation of dispatch, restargs etc.
    - Pattern matching mitigates the downsides (as in ML)
* A function consists of *methods*
    - Methods consist of a *condition* and a *body*
* A method is *applicable* to an argument if the method's condition is true
  for that argument
* A method A is more *specific* than a method B if A's condition implies B
    - That is, \(A \rightarrow B \Leftrightarrow \lnot A \lor B\) is a tautology
* When a function is *applied*
    - The argument is bound
    - The set of applicable methods is determined
        * If the set is empty, an error is signalled
    - Otherwise the most specific method is determined
        * If one cannot be found, an error is signalled
    - The body of the selected method is executed

## Pattern Matching

In Centring, a pattern is either *atomic* or *complex*.

* atomic patterns are variables or literals (`x`, `3`, `#t` etc.)
* complex patterns are applications (`(Some v)`, `(seq a b)` etc.)

Patterns can

* Perform *tests* on the matchee
    - Literals are tested for equality
    - Complex patterns often test the type or length
* *Bind* variables
    - This is all that variable patterns do
* Recursively match *subpatterns*
    - Only relevant for and usual in complex patterns

# Implementation

## Functions

The Chanbers-Chen paper "Efficient Multiple and Predicate Dispatching" has a
strategy that can be used. It doesn't handle dynamically adding methods though,
but that can be supported by keeping the function cases and reoptimizing the
function when new methods get added. (This can also be done 'lazily' so that the
actual reoptimization is performed on calls instead of redefinitions.)

## Pattern Matching

Most pattern matching happens on the argument of a function. Other pattern
matching structures can be desugared to that (for example, `match` is to the
Centring `fn` what `let` is to a Scheme `lambda`).

Furthermore, the *tests* involved in pattern matching the function argument can
be moved to the function case conditions and the *bindings* can be prefixed to
the bodies of the function cases.
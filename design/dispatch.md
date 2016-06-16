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

## Pattern Matching

This can be done with macros.

```
(def match-code
  (fn
    (#(pat name cond env) (literal? pat)
     #(`(and ,cond (centring.lang/= ,(lookup env name) ,pat)) env))

    (#(pat name cond env) (: pat Symbol)
     #(cond (prepend #(pat (lookup env name)) env)))

    (#((and (patseq T & fields) pat) name cond env)
     (let ((cond*
            `(and ,cond
                  (centring.lang/: ,(lookup env name) ,T)
                  (centring.lang/= (centring.intr/rlen ,(lookup env name))
                                   ,(count fields)))))
       (foldl-indexed
         (fn
           (#(#(cond env) pat i)
            (let ((n (gensym))
              (match-code pat n cond
                          (prepend #(n `(centring.intr/rref ,(lookup env name) i))
                                   env)))))
          #(cond* env) fields)))))
       
```
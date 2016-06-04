# Semantics

## Semantics Components

1. Control String (code & constants)
2. Lexical environment (locals & clovers)
    * Immutable
3. Top-level environment (= the current namespace)
    * (Potentially) mutable

# Abstract machine Components

1. Executing Procedure
2. Namespace registry
3. Current namespace
4. GP Registers

## Conceptual Execution Model

* Control array contains primop functions and argument fetch descriptors.
* We basically `control[pc++](fiber)` and then the primop function fetches its
  conceptual args using `control[pc++]`. Then the primop does its actual thing.
  After this, `control[pc++]` will yield another primop.
* Primops are defined in the VM implementation language.
* The control array is derived from the Centring source program. It is obvious
  that Centring programs are nothing more than compositions of these primitives.
* `call` is an extensible primitive and allows function definition, method extension.

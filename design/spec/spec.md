# Functions

    (fn name : <symbol>?
        cases : <case>+)

    <case> : (<pattern> <condition>
              body : <expr>*)

    (ctr.sf/fn name : <symbol>? arg : <symbol>
      cases : (<condition> body : <expr>*))

# Callables

## Functions

1. The applicable methods are determined. A method is applicable if its formal
   pattern matches the argument and its condition is satisfied for the argument.
2. The applicable methods are sorted based on the override order. The method A
   overrides the method B if B's pattern and condition are logically implied by
   those of A.
3. If there is a single most specific method, the bindings implied by that method's
   formal pattern are done and it's body evaluated. Otherwise an error is signaled.

# Data

# Types

## Record Types

<!-- TODO: field inheritance, parametric types -->

### Definitions

    (defrecord (tspec : <type-spec>
                rspecs : <require-spec>*
                field-specs : <field-spec>*
                rest-field-spec : (... <field-spec>)?)
      body : <expr>*)

    <type-spec> : tname : <symbol>
                | (tname : <symbol>
                   pspecs : <symbol>*
                   rest-pspec : (... <symbol>)?)

    <require-spec> : (use <symbol>)
                   | (only <require-spec> <symbol>*)
                   | (except <require-spec> <symbol>*)
                   | (prefix <require-spec>)
                   | (rename <require-spec> (<symbol> <symbol>)*)
                   | (-> <require-spec> <require-spec'>*)

    <field-spec> : fname : <symbol>
                 | (mut fname : <symbol>)

`tname` will be bound to a type object. See the section on type objects.

In the `body` `new` will be bound to a constructor function that takes
$||field-specs||$ arguments if `rest-field-spec` is not specified and
$\ge ||field-specs||$ arguments if it is specified and constructs an instance of
the type `tname`.

A `field-spec` of the form `<symbol>` specifies an immutable field. `fname`
will be bound to a getter function of the field in `body`.

A `field-spec` of the form `(mut <symbol>)` specifies a mutable field. `fname`
will be bound to a getter function of the field in `body`. That getter also has
a setter so that `(set! (,fname obj) val)` maybe used in `body`.

The `pspec`:s will be bound to a function that get the corresponding parameter
from a `tname` type.

The implementation is not required to let these values escape the `defrecord`
form.

Apart from these variables being bound in `body`, it is treated as if it was on
the same level as the `defrecord`.

### Type Objects

## Byte Types

# Modules

    (module mname : <type-spec>
      (require rspecs : (<symbol> | <require-spec>)*)?
      body : <expr>*)

Parametric and non-parametric modules can be turned into singleton record types.
`require` will check if an instance of the module already exists. If it does,
that will be used. If not, it will

1. locate the source file of the corresponding module (in an
   implementation-specific manner)
2. `include` it
3. parameterize it (if applicable)
4. instantiate it

After an instance of the module has been created, it is fairly straightforward to
handle the import operations in the `require-spec`:s.

# Syntax Derivations

## begin

    (begin stmts : <expr>*)

becomes

    ((fn (_ #t ,@stmts)))

## module

    (module foo.bar
      (require (only foo.baz quux))
      (define a 3)
      (define b 5))

becomes

    (do
      (def foo.baz (ctr.intr/require! 'foo.baz))
      (def quux (. foo.baz quux))
      (def a 3)
      (def b 5)
      (defrecord (foo.bar a b)
        (new foo.bar a b)))

and

    (module (foo.bar baz)
      (require (only baz quux))
      (define a 3)
      (define b 5))

becomes

    (fn (baz)
      (def quux (. baz quux))
      (def a 3)
      (def b 5)
      (defrecord (foo.bar a b)
        (new foo.bar a b)))

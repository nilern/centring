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

There are two kinds of types: **bits types** and **aggregate types**.

## Bits Types

Bits types have no internal structure; they are just bags of bits.

    (defbits name: <symbol> size: <nat>)

Each bits type has

* A type
* A name : Symbol
* A size : UInt (in bits). All instances of the type use that many contiguous
  bits to store their data. Implementations are not required to support sizes
  that are not multiples of 8 (one byte).
* An alignment : UInt

## Record Types

<!-- TODO: parametrics -->

Record types contain values of other types which may be atomic or aggregate.

    (defrecord name: <symbol>
       fspecs: <field-spec>*
       rfspec: <idx-spec>?)

    <idx-spec> : <field-spec>
               | (... <field-spec*>)

    <field-spec> : <access-spec>
                 | (: <access-spec> <expr>)
                 | (include <expr>)

    <field-spec*> : <access-spec>
                  | (: <access-spec> <expr>)

    <access-spec> : fname : <symbol>
                  | (mut fname : <symbol>)

Record types can be defined like the following (but implementations can actually
store this information any way they like):

    (defrecord RecordType
      (: name Symbol)
      (: fixed? Bool)
      (... (: fields FieldDescr)))

    (defrecord RecordType.Flat
      (: size UInt)
      (: align UInt)
      (include RecordType))

    (defrecord FieldDescr
      (: name Symbol)
      (: mutable? Bool)
      (: type (Option Type)))

    (defrecord FieldDescr.Flat
      (include FieldDescr)
      (: offset UInt))

Each record type has

* A type (like any first-class value. not a normal field)
* A name
* A size staticality (`fixed?`)

If the record type is flat, it will also have

* A (minimum) size (in bytes)
* An alignment (in bytes)

Record types consist of fields. Each field has

* A name
* A mutability (defaults to false = immutable)
* A type (optional, fields may be untyped)

Fields of flat record types also have

* A byte offset : UInt (in bytes)

The final field of in a record type may be an **indexed field**. If the type has
an indexed field, it is **dynamically sized**. Otherwise it is **statically
sized**.

Records support multiple inheritance of data. The fields of other record types
can be included in the record definition and even interspersed with fresh field
definitions. A dynamically sized type can only be `include`:d at the very end so
that its indexed field will be the last field in the new type.

Note that field inheritance creates no subtype relationship and functions
dispatching by type will be oblivious to this inheritance. The fields are simply
copied so there is also no diamond inheritance problem.

## Memory Representation

A type is said to be **flat** when its values contains no value references^[that
is, runtime-managed pointers]. A flat type should be stored as a contiguous byte
blob in a C-compatible way. Non-flat types have no such requirements on their
memory representation and a simple array of value references is sufficient.

* All bits types are flat^[even ones that contain raw pointers obtained from
  e.g. C libraries].
* A record type is flat if all the types of its fields are flat and
  fully immutable. If the type has a field that has a dynamically sized type it
  will not be flat unless that field is the last field in the record and it is
  not an indexed field. If the type has any untyped field it will not be flat.
    * A type is **fully immutable** if
        * It is a bits type or
        * It is a record type that has only immutable fields

Note that a flat record type can include mutable fields -- those fields just
can't store mutable values. C/C++/Rust structs that contain other structs can be
modelled by using field inheritance.

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

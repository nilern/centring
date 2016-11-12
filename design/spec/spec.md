# Fundamentals

## Syntax

S-expressions:

* A `Seq` of S-expressions
* Or an atom

## Special Forms

* fn        -- create function
* do        -- treat expressions as statements
* def       -- add a binding to the enclosing scope
* defsyntax -- add a macro binding to the enclosing scope
* module    -- create an applicative module functor
* module*   -- create a generative module functor
* import    -- import from modules
* meta      -- do things at expansion time
* syntax    -- prevent evaluation, leave in syntax object
* quote     -- prevent evaluation, leave in contents of syntax object

## Operations

* Functions
    * Application (`apply`)
* Data
    * Construction (`new`)
    * Field access and mutation (`.x`, `.x-set!`, `set!`, `.`)
    * Iteration (`first`, `rest`)
* I/O (file, user input, threads)
    * Send and receive (`>!`, `<!`)
* (Intrinsics for addition, fn merging etc., hidden; implementation-dependent)

# Special Forms

## `fn`

    (fn name : <symbol>?
        cases : <case>+)

    <case> : (<pattern> <condition>
              body : <expr>*)

## `do`

    (do <stmts:expr*>)

## `def`

    (def <name:symbol> <value:expr>)

## `defsyntax`

    (defsyntax <name:symbol> <value:expr>)

## `module` and `module*`

    (module <mod-kind> <body: expr*>)

    (module* <mod-kind> <body: expr*>)

    <mod-kind> : (<Symbol> <dependencies: Symbol*>)
               | <Symbol>

## `import`

    (import <import-spec*>)

    <import-spec> : <mod-kind>
                  | (prefix <import-spec> <name: Symbol>)
                  | (only <import-spec> <Symbol*>)
                  | (except <import-spec> <Symbol*>)
                  | (rename <import-spec> (<old-name: Symbol> <new-name: Symbol>)*)

## `meta`

    (meta <stmts:expr*>)

## `syntax`

    (syntax <expr>)

## `quote`

    (quote <expr>)

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

## Fundamental Operations

* Construction (`new`, constructor functions)
* Selection (`.x`, selector functions)
* Mutation (`.x-set!`, `set!`)
* Destruction (garbage collection)

## Types

There are two kinds of types: **bits types** and **aggregate types**.

### Bits Types

Bits types have no internal structure; they are just bags of bits.

    (defbits name: <symbol> size: <nat>)

Each bits type has

* A type
* A name : Symbol
* A size : UInt (in bits). All instances of the type use that many contiguous
  bits to store their data. Implementations are not required to support sizes
  that are not multiples of 8 (one byte).
* An alignment : UInt

### Record Types

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

#### Memory Representation

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

### Type Constructors

<!-- Here it is assumed that (= (: #()) Array), not ... Tuple) -->

A **type constructor** is a function that takes some types and returns a type on
`apply`:

    (Tuple Int Bool) ; => ~ (isize, bool)

<!-- Would be useful to also take `mut`, `...` and numbers. Maybe we could have
     special type constructors Mut and Splat..? -->

Many type constructors also support type inference and `new`:

    (: (new Some 2) (Some Int))

# Type Classes / Capabilities

# Code Organization

## `include`

The simplest and most low-level option is `include`, which just copies and
pastes a file. It does use the `$CTR_PATH` mechanism to locate the file.

If the file and read function haven't changed it is of course unnecessary to
re-read the file so implementations might perform caching of S-expressions in
this case.

## Modules

The `module` and `module*` forms are used to define module functors. A module
functor can be *instantiated* to obtain a module by giving it the required
number of other module names or instantiations as arguments. The parentheses can
be omitted for zero-argument module functors.

<!-- Would be nice to be able to give non-module arguments but there might not
     be a clean way to do that -->

The difference between the `module` and `module*` forms is that `module` creates
applicative functors and `module*` creates generative functors. Thus `module` is
more efficient to instantiate and generates less code. Sometimes it is desirable
to use `module*`, for instance if multiple instantiations of stateful modules
are required.

# Syntax Derivations

## begin

    (begin stmts : <expr>*) --> ((fn (_ #t ,@stmts)))

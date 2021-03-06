## Compilation Units

After macroexpansion:

    <compilation_unit> ::= <decl>*
    <decl> ::= <ns> | <require> | <def>
    <ns> ::= (centring.intr/set-ns! (quote NS_NAME))
    <require> ::= (centring.intr/require! (centring.sf/quote NS_NAME)*)
                | (centring.intr/alias! (centring.sf/quote NS_NAME)
                                        (centring.sf/quote NS_NAME))
                | (centring.intr/refer! (centring.sf/quote NS_NAME)
                                        (centring/sf.quote SYMBOL)*)
    <def> ::= (centring.intr/set-global! BOOL (centring/sf.quote SYMBOL_NAME)*
                                         <expr>)
            | (centring.intr/add-method! <expr> <expr>)

<!-- How about FFI? -->

## Compilation Modes

1. In **development mode** runtime namespaces are enabled; all references to
globals even in the binary itself go through the namespace mechanism so globals
(even private ones) can be redefined.
2. In **release mode** runtime namespaces are disabled. Now globals are resolved
directly and redefinitions are impossible. Private globals can be optimized.
Public globals cannot be optimized since their call sites and mutations (adding
methods, for example) are unknown.
3. The **whole-program mode** can even optimize public globals since it sees the
external call-sites and mutations as well.

<!-- Implementation should probably start with release mode -->

## Headers

There will be some kind of header format to store

1. Macros
2. Namespace information

# Data and Types

## Bits Types

* Int
* Float
* Bool
* Char

## Record Types

    (defrecord (Point x y))

    (defrecord (Tuple (... vals)))

    (defrecord (Array (... (mut vals))))

    (defrecord (Closure proc (... (mut vals))))

<!-- parametric types? -->

## Kinds

    (defkind Any)

    (defkind (Callable T)
      (call T (... Any)))

    (defkind (Functor T)
      (fmap Callable T))

<!-- inheritance (i.e. Monad <: Applicative <: Functor)? -->

## Variants

# Code and Callables

Like all Lisps, Centring is based on **application**. In very general terms, an
(unquoted) list is interpreted in this manner:

    (let ((code '(foo bar baz quux))
          (((List.Pair op args) (map eval code)))
      (apply op args))

In Centring as in other Lisps there are actually multiple variations on this
theme.

## Macros

Macros are expanded in the macroexpansion phase. A macro receives an unevaluated
S-expression and returns another. This results in a sort of call-by-name
evaluation. This is necessary since macros often expand into forms that are also
macro calls.

* In Centring, there are both function-macros and symbol-macros.
* Macroexpansion is hygienic (using the sets-of-scopes approach).

## Special Forms

Special forms implement fundamental aspects of the language such as

* fn
* letrec
* call-cc

They reside in the pseudo-namespace `centring.sf` and are specially handled by
the compiler (and in fact form the backbone of the IR).

Since they don't exist at runtime, special forms can only be directly applied.

## Intrinsics

Intrinsics represent fundamental operations that are provided by the execution
platform such as

* iadd
* record (allocate record)
* brf (implements `if`)
* call

They reside in the pseudo-namespace `centring.intr` and are handled by the
compiler (mostly in the code generation phase).

Since they don't exist at runtime, intrinsics can only be directly applied.

## Multimethods

* eval callee and args, perform splatting
* select most specific method of callee based on args' values/types/kinds
  - panic if none found
* bind formals to args
* jump to body

<!-- predicate dispatch? -->

### Efficient implementation

Common cases can be optimized:

0. Multimethods (Multiple methods, polymorphic callsites etc.)
1. Closures (Function has just one method.)
2. Procedures (The one method doesn't close over anything.)
3. Contification (All calls of the function return to the same location.)
4. Inlining (Method at callsite is known and cannot change.)

=> think in terms of Callable, not Function/Closure/Procedure/Constructor...

*Escaping functions must be multimethods to allow possible extension.*

## Callable

can be implemented for any type.

---

# Datatypes

## Built-In Types

### Immediate

* Int (31-bit signed integer)
* Bool (Bool.True and Bool.False)
* Char (28-bit codepoint)
* Void (void/()/nil, used by e.g. `(do)`)

### Composite

* Tuple (immutable anonymous product type)
* Array (mutable anonymous product type)
* Buffer (just a bunch of bytes, mostly for FFI)

## Product Types

### With Indexed Fields

Tuples are immutable collections with zero-based indexing:

    (def tup #(1 2 3))
    (get tup 1) ;=> 2

Arrays are their mutable counterparts:

    (def arr #[a b c])
    (get arr 1) ;=> 2
    (set! arr 1 4) ;=> #=(None), arr is now #[1 4 3]

On the other hand, Arrays are bad for e.g. HashMap keys since they can be
mutated (they could be hashed as their pointer, which may or may not be
what you want).

Both are fixed-length but can be use to implement variable length immutable
or mutable collections.

### With Named Fields

You can define new Record types with named fields:

    (defrecord (Point x y))
    ;; Setter
    (fmap .x (list (Point 2 3) (Point 3 4))) ;=> '(2 3)

If you wish, some fields can be made mutable:

   (defrecord (Box (mut val)))
   (def counter (Box 0))
   (set! (.x counter) (inc (.x counter))) ; counter.x is now 1

Singletons are useful for some things, especially implementing enums:

    (defsingleton (None))
    (identical? (None) (None)) ;=> #t

## Kinds

Kinds are abstract types (they can be supertypes but not instantiated):

   (defkind Foldable)

In fact, Records and Kinds can only have Kinds as supertypes. Multiple
inheritance of Kinds is supported.

Interfaces are 'checked Kinds', you need to implement something to add them
to your supertypes:

   (definterface (Foldable T)
     (fold f v t: T))

## Sum Types

Tagged enums are sugar for Kinds and Records:

    (deftype List
      (Pair left right)
      (Empty))

    ;; macroexpands to:

    (do
      (defkind List)
      (defrecord (List.Pair left right) List)
      (defrecord (List.Empty) List))

Unions represent a set of arbitrary types:

    (Union List Array)

# Callables

## Fns

These are just good ol' lambdas:

    (fn (x) x)

But with the added benefit of argument types, which are checked at call-time
and can also be used at compile-time for type-checking and optimization:

    (fn (a :Int b :Int) (+ a b))

## NativeFns

These work like Fns but actually wrap a function of the implementation
language or FFI target.

## MultiFns

These are multimethods. They hold methods that are (Native)Fns. The Fns' type
information is used for the dispatching.

    (def (fmap f ls :List)
      (foldr (fn (v acc) (cons (f v) acc)) '() ls))

    (def (fmap f arr :Array)
      (let (new-arr (array (count arr)))
        (doseq (#(i v) (enumerate arr))
          (set! new-arr i (f v)))
        new-arr))

Methods can be added 'manually':

    (add-method! mbind (fn (n: Option.None f) n))

But usually one uses `def` as above with `fmap`. Adding a method to a Fn or
NativeFn will first wrap the old fn in a MultiFn.

## Making Other Things Callable

If a non-*Fn is called, the MultiFn `call` will be used:

    (def (call tup :Tuple i)
      (get tup i))
    (#(1 2 3) 1) ;=> 2

# Bindings

The basic variable definer is `def`:

   (def foo 5) ;=> #=(None), foo is now 5
   (def (identity x) x) ; identity is now (fn (x) x)

Like Scheme, function scope is used but sugared over to be lexical scope.
Internal `def`s are ok too.

# Loops

Tail-call optimization will be performed (via CPS transformation). Named
`let`:s are also available:

    (let loop (i 0)
      (when (< i 10)
        (println i)
        (loop (inc i))))

The Clojure-like 'loop-recur' is further sugar:

    (loop (i 0)
      (when (< i 10)
        (println i)
        (recur (inc i))))

Of course these just desugar into

   (do
     (def (loop i)
       (when (< i 10)
         (println i)
         (loop (inc i))))
     (loop 0))

# Data Literals

    '(1 2 3)    ; List (singly-linked list)
    [1 2 3]     ; Vector (Bitmapped HashTrie with tail)
    {:a 2 :b 4} ; HashMap (Bitmapped HashTrie)
    #(1 2 3)    ; Tuple (like an array, but immutable)
    #[1 2 3]    ; Array (like a Scheme vector)
    #{2 3 1}    ; HashSet (Bitmapped HashTrie)

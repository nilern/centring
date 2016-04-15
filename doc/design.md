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

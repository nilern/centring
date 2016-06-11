# Primitive Operations

The primops are given here with predicate expressions. The predexprs are
actually found in the methods wrapping the primops. This way all checks are made
by `call` and the other primops can be simple and fast.

# Namespace Management

\((ns) \rightarrow \kappa() \quad\mathrm{where}\quad ns: Symbol\)

    (%set-ns! ns [($cont () ...)])

    (%require! ns [($cont () ...)])
Set the current namespace to the one named by `ns`.

Load the code for the namespace named by `ns` if necessary.
<!-- What does 'necessary' mean? When it hasn't been loaded yet? When the file
has changed on disk? -->

\((ns, as) \rightarrow \kappa()
  \quad\mathrm{where}\quad ns: Symbol \land as: Symbol\)

    (%alias! ns-name as [($cont () ...)])
Create an alias called `as` in the current namespace for the namespace named `ns`.

\((ns, n, as) \rightarrow \kappa()
  \quad\mathrm{where}\quad ns: Symbol \land n: Symbol \land as: Symbol\)

    (%rename! ns n as [($cont () ...)])
Create an alias called `as` in the current namespace for the var `ns/n`.

\((ns) \rightarrow \kappa() \quad\mathrm{where}\quad ns: Symbol\)

    (%import! ns [($cont () ...)])
Import all the vars in the namespace named by `ns` into the current namespace.

\((n, v) \rightarrow \kappa() \quad\mathrm{where}\quad n: Symbol\)

    (%set-global! n v [($cont () ...)])
Set the value of the var called `n` in the current namespace to `v`.

\((n, v) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad n: Symbol \land d: Bool\)

    (%defined? n [($cont (d) ...)])

Will dereferencing the var called `n` yield a value?

<!-- These should probably say `(? (unqualified-symbol? ns-name))` or sth. -->
<!-- Need to address public/private and syntax/runtime distinctions -->

# Records

    (%rec t vs...)


    (%srec)

\((r) \rightarrow \kappa(l) \quad\mathrm{where}\quad r::Record \land l: Int\)

    (%rlen r)


    (%rref r i)


    (%rset! r i v)

# Arithmetic

\((a, b) \rightarrow \kappa(d)|\bot(Overflow|DivideByZero...)
  \quad\mathrm{where}\quad a: Int \land b: Int \land d: Int\)

    (%iadd a b [($cont (d) ...)])
    (%isub a b [($cont (d) ...)])
    (%imul a b [($cont (d) ...)])
    (%idiv a b [($cont (d) ...)])
    (%irem a b [($cont (d) ...)])
    (%imod a b [($cont (d) ...)])

+, -, *, /, rem, and mod.

\((a) \rightarrow \kappa(d)|\bot(Overflow)
  \quad\mathrm{where}\quad a: Int \land d: Int\)

    (%ineg a [($cont (d) ...)])

Unary `-`.

# Bit operations

\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a: Int \land b: Int \land d: Int\)

    (%iand a b [($cont (d) ...)])
    (%iior a b [($cont (d) ...)])
    (%ixor a b [($cont (d) ...)])
    (%iash a b [($cont (d) ...)])
    (%ilsh a b [($cont (d) ...)])

&, |, ^, `<</>>`, and `<<</>>>`.

\((a) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a: Int \land d: Int\)

    (%inot a [($cont (d) ...)])

~.

# Comparisons

\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a: Int \land b: Int \land d: Bool\)

    (%ieq a b [($cont (d) ...)])
    (%ine a b [($cont (d) ...)])
    (%igt a b [($cont (d) ...)])
    (%ige a b [($cont (d) ...)])
    (%ilt a b [($cont (d) ...)])
    (%ile a b [($cont (d) ...)])

=, !=, >, <, >= and <=.

# Branches

\((c) \rightarrow \kappa_1()|\kappa_2()
  \quad\mathrm{where}\quad c: Bool\)

    (%brf c ($cont () ...1) ($cont () ...2))

If c is `True`, evaluate `...1`. Else evaluate `...2`.

<!-- If this is used to implement dispatch, how to ensure `(: c Bool)`? -->

\((f, as...) \rightarrow \top(d)|\bot(UnCallable|ArgCount|NoMethod...)
  \quad\mathrm{where}\quad f::Callable\)

    (%call f & as)

1. Check that `f` is `Callable`
    * Panic if not.
2. Compute the set of arity-compatible methods in `f`.
    * Panic if the set is empty.
3. Bind the formal parameters and compute the set of applicable methods in `f`.
    * Panic if the set is empty.
4. Select the most specific method (the one that *implies* all the other
   applicable methods).
    * Panic f there is no single most specific method.
5. Execute the body of the selected method.

*"in `f`" is taken to mean the methods of `f` (if `(: f Fn)`) or the methods of
  `centring.lang/call` pertaining to `f` otherwise.*

\((f, a) \rightarrow \top(d)|\bot(UnCallable|ArgCount|NoMethod...)
  \quad\mathrm{where}\quad f::Callable\)

   (%apply f a)

Like `%call` but just uses `a` as the argument (i.e. doesn't construct a tuple
in secret).
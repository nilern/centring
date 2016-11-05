# Data Representation

## Basic Types

    (defbits Int ##sys#word-size)

    (defbits UInt ##sys#word-size)

    (defbits UInt8 8)

    (defbits VoidPtr ##sys#ptr-size)

    (defrecord String
      (... (: bytes UInt8)))

    (defrecord Symbol
      (... (: bytes UInt8)))

    (defrecord ArrayMut
      (... (mut elems)))

## Internal Types

    (defrecord Env
      parent
      (: count UInt)
      (: buckets ArrayMut))

    (defrecord EnvBucket
      next
      (: key Symbol)
      value)

    (defrecord SourceInfo
      (: line UInt)
      (: column UInt)
      (: filename String))

### AST

    (defrecord Fn
      (include SourceInfo)
      (: name Symbol)
      (: parameter Symbol)
      (... cases))

    (defrecord App
      (include SourceInfo)
      op
      (... args))

    (defrecord Def
      (include SourceInfo)
      (: name Symbol)
      value)

    (defrecord Expr
      (include SourceInfo)
      (: op VoidPtr)
      (... args))

    (defrecord Stmt
      (include SourceInfo)
      (: op VoidPtr)
      (... args))

    (defrecord Ctrl
      (include SourceInfo)
      (: op VoidPtr)
      determinant
      (... branches))

    (defrecord Closure
      (include SourceInfo)
      (: env Env)
      body)

    (defrecord Do
      (include SourceInfo)
      (... statements))

    (defrecord Var
      (include SourceInfo)
      (: name Symbol))

    (defrecord Const
      (include SourceInfo)
      value)

### Continuations

    (defrecord FnCont
      parent
      (: ast App)
      (: env Env))

    (defrecord ArgCont
      parent
      (: ast App)
      (: env Env))

    (defrecord DefCont
      parent
      (: name Symbol)
      (: env Env))

    (defrecord ExprCont
      parent
      (: ast Expr)
      (: index UInt)
      (: env Env)
      (... vals))

    (defrecord StmtCont
      parent
      (: ast Stmt)
      (: index UInt)
      (: env Env)
      (... vals))

    (defrecord CtrlCont
      parent
      (: ast Ctrl)
      (: env Env))

    (defrecord DoCont
      parent
      (: ast Do)
      (: index UInt)
      (: env Env))

    (defrecord HaltCont)

# Special Forms

(Prefixed by ##sf#):

* fn    -- create Fn (a predicate-dispatching closure)
* apply -- call functions (or other things, e.g. types as constructors)
* do    -- treat expressions as statements
* def   -- add a binding to the current environment frame
* meta  -- do things at expansion time
* syntax -- prevent evaluation, leave in syntax object
* quote -- prevent evaluation, leave in contents of syntax object

## Fn

    (##sf#fn name : <symbol> formal : <symbol> cases : (condition body)*)

### Expansion time

Create a fresh scope. Add it to and expand subforms. Reassemble the `fn` form
from those.

### Analysis time

`Fn name formal (Array.of_list_map ~f:analyze_case cases)`.

where `analyze_case` turns `[condition; body]` into
`(dnf (analyze cond), analyze body)`.

### Runtime

Create a Fn.

## Apply

    (##sf#apply callee arg)

### Expansion time

Just recursively expanded.

### Analysis time

`App (analyze callee) (analyze arg)`.

### Runtime

Perform call.

## Do

    (##sf#do stmts*)

### Expansion time

Just recursively expanded.

### Analysis time

`Do (Array.of_list_map ~f:analyze stmts)`.

### Runtime

Execute `stmts` in sequence.

## Def

    (##sf#def name : <symbol> expr)

### Expansion time

Expand `name` and `expr`. Remove any use-site scopes from `name`.

### Analysis time

`Def name (analyze expr)`.

### Runtime

Evaluate `expr` and associate the result with `name` in the current environment
frame.

## Meta

    (##sf#meta expr)

### Expansion time

Increment `phase`, then expand and eval expr, leaving `(##sf#do)` in its place.
Restore the previous value of `phase` afterwards.

### Analysis time

It is an error if this occurs at analysis time.

### Runtime

Since `expr` was executed at expansion time there is nothing left to do but
return `#()`.

## Syntax

    (##sf#syntax expr)

### Expansion time

Return the form unchanged.

### Analysis time

`Const expr`.

### Runtime

Return the constant.

## Quote

    (##sf#quote expr)

### Expansion time

Return the form unchanged.

### Analysis time

`Const (%stx-expr expr)`.

### Runtime

Return the constant.

# Primitive Operations

The primops are given here with predicate expressions. The predexprs are
actually found in the methods wrapping the primops. This way all checks are made
by `call` and the other primops can be simple and fast.

# Namespace Management

<!-- %\((ns) \rightarrow \kappa() \quad\mathrm{where}\quad ns: Symbol\) -->

    (%set-ns! ns [($cont () ...)])

    (%require! ns [($cont () ...)])
Set the current namespace to the one named by `ns`.

Load the code for the namespace named by `ns` if necessary.
<!-- What does 'necessary' mean? When it hasn't been loaded yet? When the file
has changed on disk? -->

<!-- %\((ns, as) \rightarrow \kappa()
  \quad\mathrm{where}\quad ns: Symbol \land as: Symbol\) -->

    (%alias! ns-name as [($cont () ...)])
Create an alias called `as` in the current namespace for the namespace named `ns`.

<!-- %\((ns, n, as) \rightarrow \kappa()
  \quad\mathrm{where}\quad ns: Symbol \land n: Symbol \land as: Symbol\) -->

    (%rename! ns n as [($cont () ...)])
Create an alias called `as` in the current namespace for the var `ns/n`.

<!-- %\((ns) \rightarrow \kappa() \quad\mathrm{where}\quad ns: Symbol\) -->

    (%import! ns [($cont () ...)])
Import all the vars in the namespace named by `ns` into the current namespace.

<!-- %\((n, v) \rightarrow \kappa() \quad\mathrm{where}\quad n: Symbol\) -->

    (%set-global! n v [($cont () ...)])
Set the value of the var called `n` in the current namespace to `v`.

<!-- %\((n, v) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad n: Symbol \land d: Bool\) -->

    (%defined? n [($cont (d) ...)])

Will dereferencing the var called `n` yield a value?

<!-- These should probably say `(? (unqualified-symbol? ns-name))` or sth. -->
<!-- Need to address public/private and syntax/runtime distinctions -->

# Records

    (%rec t vs...)


    (%srec t vs...)

<!-- %\((r) \rightarrow \kappa(l) \quad\mathrm{where}\quad r::Record \land l: Int\) -->

    (%rlen r)


    (%rref r i)


    (%rset! r i v)

    (%type r)

# Arithmetic

<!-- %\((a, b) \rightarrow \kappa(d)|\bot(Overflow|DivideByZero...)
  \quad\mathrm{where}\quad a: Int \land b: Int \land d: Int\) -->

    (%iadd a b [($cont (d) ...)])
    (%isub a b [($cont (d) ...)])
    (%imul a b [($cont (d) ...)])
    (%idiv a b [($cont (d) ...)])
    (%irem a b [($cont (d) ...)])
    (%imod a b [($cont (d) ...)])

+, -, *, /, rem, and mod.

# Bit operations

<!-- %\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a: Int \land b: Int \land d: Int\) -->

    (%iand a b [($cont (d) ...)])
    (%iior a b [($cont (d) ...)])
    (%ixor a b [($cont (d) ...)])
    (%iash a b [($cont (d) ...)])
    (%ilsh a b [($cont (d) ...)])

&, |, ^, `<</>>`, and `<<</>>>`.

<!-- %\((a) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a: Int \land d: Int\) -->

    (%inot a [($cont (d) ...)])

~.

# Boolean Operations

Anything except `#f` is considered truthy.

<!-- %\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a::Any \land b::Any \land d: Bool\) -->

    (%band a b [($cont (d) ...)])
    (%bior a b [($cont (d) ...)])
    (%bxor a b [($cont (d) ...)])

<!-- Are these actually needed aside from the IR representation of Fn case
 conditions? -->

<!-- %\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a::Any \land d: Bool\) -->

    (%bnot a [($cont (d) ...)])

# Comparisons

<!-- %\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a: Int \land b: Int \land d: Bool\) -->

    (%ieq a b [($cont (d) ...)])
    (%ine a b [($cont (d) ...)])
    (%igt a b [($cont (d) ...)])
    (%ige a b [($cont (d) ...)])
    (%ilt a b [($cont (d) ...)])
    (%ile a b [($cont (d) ...)])

=, !=, >, <, >= and <=.

<!-- %\((a, b) \rightarrow \kappa(d)
  \quad\mathrm{where}\quad a::Any \land b::Any \land d: Bool\) -->

    (%bit-eq? a b)

Pointer/bit equality (like Scheme `eq?`).

# Branches

<!-- %\((c) \rightarrow \kappa_1()|\kappa_2()
  \quad\mathrm{where}\quad c::Any\) -->

    (%brf c ($cont () ...1) ($cont () ...2))

If c is not `#f`, evaluate `...1`. Else evaluate `...2`.

<!-- %\((f, as...) \rightarrow \top(d)|\bot(UnCallable|ArgCount|NoMethod...)
  \quad\mathrm{where}\quad f::Callable\) -->

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

<!-- %\((f, a) \rightarrow \top(d)|\bot(UnCallable|ArgCount|NoMethod...)
  \quad\mathrm{where}\quad f::Callable\) -->

   (%apply f a)

Like `%call` but just uses `a` as the argument (i.e. doesn't construct a tuple
in secret).

# Functions

## Resolving Overrides

### dispatch.ml

1. Generate an override matrix : [[Option<AST>]]
    * m1 == m2
      => `overrides[i1, i2] = None`
    * Can statically determine that m1 overrides m2
      => `overrides[i1, i2] = Some(Const(True))`
    * m1 overrides m2 if a guard is true at runtime
      => `overrides[i1, i2] = Some(guardExpr)`
    * Can statically determine that m2 overrides m1
      => `overrides[i1, i2] = Some(Const(False))`
    * What about the case when we can determine nothing? Is it `Const(False)`?
2. Generate something like the following:

        ($def mmis (($closure _ max-method-indices)
                    (fn
                      (#(i1 i2) #t
                       (%switch i1
                         (%switch i2
                           ...)
                         ...)))
                    ,l '() 0))
        (%brf (%identical? (%type (%cdr mmis)) List.Empty)
          (%switch (%car mmis) ($closure ,env ,body) ...)
          (%err 'AmbiguousMethodError ...))

    where

    1. the first ellipses are filled in based on the override matrix
    2. `l` is the number of methods whose overrides are being resolved
    3. The third ellipsis is filled in with the method bodies
    4. The fourth ellipsis should have some useful info like method name and
       source position

### bootstrap0.ctr

#### High Level

    (defn max-method-indices
      (#(method-cmp n) #t
       (defn cmp-all
         (#(mmis i) (empty? mmis) ; nothing to compare i against?
          #{i})                   ; put i in the set.
         (#(mmis i) #t
          (defn step
            (#(ord mmi) (= (method-cmp i mmi) ord) ; still the same result?
             ord)                                  ; keep it.
            (#(_ _) #t                             ; results disagree?
             (Unknown)))                           ; inconclusive.
          (match (foldl step (method-cmp i (first mmis)) (rest mmis))
            ((Left) #t mmis)             ; ignore i, it can't win
            ((Unknown) #t (conj mmis i)) ; put i in the set
            ((Right) #t #{i}))))         ; i overrode the whole set, discard those
       (foldl cmp-all #{} (range n))))

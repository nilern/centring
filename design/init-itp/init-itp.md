# Data Representation

# AST

    type ast = Fn Symbol.t Symbol.t [|(condition, ast)|]
             | App ast ast
             | Def Symbol.t ast
             | Primop primop [|ast|] [|ast|]
             | Closure env ast
             | Do [|ast|]
             | Id Symbol.t
             | Const value

# Primops

    type primop = Expr string ([|value|] -> value)
                | Stmt string ([|value|] -> ())
                | Ctrl string ([|value|] -> [|ast|] -> ast)

# CEK

    type cont = Fn ast env cont
              | Arg value env cont
              | Def Symbol.t env cont
              | Primop primop value list [|ast|] int [|ast|] env cont
              | Do [|ast|] int env cont
              | Halt

# Special Forms

(Prefixed by ##sf#):

* fn    -- create Fn (a predicate-dispatching closure)
* apply -- call functions (or other things, e.g. types as constructors)
* do    -- treat expressions as statements
* def   -- add a binding to the current environment frame
* meta  -- do things at expansion time
* quote -- prevent evaluation (low-level, does not handle lists etc.)

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

    (##sf#meta phase-increment : <uint> expr)

### Expansion time

Increment `phase` by `phase-increment`, then expand and eval expr, leaving
`(##sf#do)` in its place. Restore the previous value of `phase` afterwards.

### Analysis time

It is an error if this occurs at analysis time.

### Runtime

Since `expr` was executed at expansion time there is nothing left to do but
return `#()`.

## Quote

    (##sf#quote expr)

### Expansion time

Return the form unchanged.

### Analysis time

`Const expr`.

### Runtime

Return the constant.


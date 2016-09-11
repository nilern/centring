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

    type primop = Expr string ([|value|] => value)
                | Stmt string ([|value|] => ())
                | Ctrl string ([|value|] => [|ast|] => ast)

# CEK

    type cont = Fn ast env cont
              | Arg value env cont
              | Def Symbol.t env cont
              | Primop primop value list [|ast|] int [|ast|] env cont
              | Do [|ast|] int env cont
              | Halt

# Special Forms

(Prefixed by ##sf#):

* fn
* apply
* do
* def
* syntax
* quote

## Fn

    (##sf#fn name : <symbol> formal : <symbol> cases : (condition body)*)

### Expansion time

    new_scope = Scope.fresh (Scope.Fn nsym)

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

    (##sf#def phase : <uint> name : <symbol> expr)

### Expansion time

### Analysis time

`Def name (analyze expr)`.

### Runtime

Evaluate `expr` and associate the result with `name` in the current environment
frame.

## Quote

    (##sf#quote expr)

### Expansion time

Return the form unchanged.

### Analysis time

`Const expr`.

### Runtime

Return the constant.


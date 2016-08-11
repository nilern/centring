# Initial Interpreter Overview

## Phases

1. Parse
2. Expand
3. Analyze
4. Interpret

## Parse

`ctr-read` parses text into Scheme S-exprs.

## Expand

`expand-all` performs hard-coded macro expansions.

## Analyze

1. `analyze` converts the expanded S-expr into an AST, resolves primops and
   DNF-converts `fn` case conditions.
2. `demodularize` changes the current namespace, loads `require`:d code and
   resolves globals as needed, essentially executing `ns` and `require` and
   removing them from the AST.

Primop resolution embeds the primop object into the `Primop` node.

Global resolution embeds the `Var` object into the `Global` node.

## Interpret

`interpret` uses a CEK machine to evaluate the AST.

Thanks to `demodularize`, the CEK machine does not need to deal with the special
nature of namespaces (= global environments).
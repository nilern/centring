open Core.Std
module Env = Environment

let sexp_of_bytes _ = Sexp.Atom "<bytes>"

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (condition * ast) array
        | App of ast * ast
        | Def of int * Symbol.t * ast
        | Primop of primop * ast array * ast array
        | Closure of env * ast
        | Do of ast array
        | Id of Symbol.t
        | Const of value [@@deriving sexp_of]

and value = Int of int
          | Bool of bool
          | Char of char
          | Symbol of Symbol.t
          | List of value list
          | Stx of value * String.Set.t * src_info
          | FnClosure of Symbol.t * Symbol.t * fnbody ref
          | Record of value * value array
          | Bytes of value * bytes

and primop = Expr of string * (value array -> value)
           | Stmt of string * (value array -> unit)
           | Ctrl of string * (value array -> ast array -> ast)

and fnbody = Done of ast * (clause * ast * env) Sequence.t
           | Pending of (clause * ast * env) Sequence.t

and atom = Not of ast
         | Base of ast

and clause = atom array

and condition = clause array

exception CtrError of value * value [@@deriving sexp_of]

(* Accessors *)

let atom_ast = function
  | Base e -> e
  | Not e -> e

(* Traversals *)

let atom_map f = function
  | Base ast -> Base (f ast)
  | Not ast -> Not (f ast)

let rec ast_map f = function
  | Fn (name, formal, methods) ->
    let meth_map (clauses, body) =
      (Array.map ~f:(Array.map ~f:(atom_map (ast_map f))) clauses,
       ast_map f body) in
    Fn (name, formal, Array.map meth_map methods)
  | App (callee, arg) ->
    App (f callee, f arg)
  | Primop (op, args, conts) ->
    Primop (op, Array.map f args, Array.map f conts)
  | Closure (env, ast') ->
    Closure (env, f ast')
  | Do stmts ->
    Do (Array.map f stmts)
  | (Id _ as node) | (Const _ as node) ->
    node

let walk inner outer ast = outer (ast_map inner ast)

let rec postwalk f ast = walk (postwalk f) f ast

(* Conversions *)

let sexp_of_primop _ = Sexp.Atom "#<primop>"

let rec value_to_string = function
  | Int i -> Int.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Char c -> String.of_char_list ['#'; '\\'; c]
  | Symbol s -> Symbol.to_string s
  | FnClosure (name, _ , _) -> sprintf "#<Fn %s>" (Symbol.to_string name)
  | Record (t, _) -> value_to_string t
  | Bytes (t, _) -> value_to_string t

let sexp_of_value = function
  | List es -> Sexp.List (List.map es sexp_of_value)
  | Stx (e, _, _) -> Sexp.List [Sexp.Atom "Stx"; sexp_of_value e]
  | e -> Sexp.Atom (value_to_string e)

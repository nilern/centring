open Core.Std
module Env = Environment

let sexp_of_bytes _ = Sexp.Atom "<bytes>"

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

module Phase = Int

type ctx = (Scope.Set.t) Phase.Map.t

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (condition * ast) array
        | App of ast * ast
        | Def of Symbol.t * ast
        | Primop of primop * ast array * ast array
        | Closure of env * ast
        | Do of ast array
        | Id of Symbol.t
        | Const of value

and value = Int of int
          | Bool of bool
          | Char of char
          | Symbol of Symbol.t
          | List of value list
          | Stx of value * ctx * src_info
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

let rec sexp_of_value = function
  | Int i -> Int.sexp_of_t i
  | Bool b -> if b then Sexp.Atom "#t" else Sexp.Atom "#f"
  | Char c -> Sexp.Atom (String.of_char_list ['#'; '\\'; c])
  | Symbol s -> Symbol.sexp_of_t s
  | List es -> Sexp.List (List.map es sexp_of_value)
  | Stx (e, _, _) -> Sexp.List [Sexp.Atom "Stx"; sexp_of_value e]
  | FnClosure (name, _ , _) ->
    Sexp.Atom (sprintf "#<Fn %s>" (Symbol.to_string name))
  | Record (_, _) -> Sexp.Atom "#<record>"
  | Bytes (_, _) -> Sexp.Atom "#<bytes>"

let rec sexp_of_atom = function
  | Not e -> Sexp.List [Sexp.Atom "Not"; sexp_of_ast e]
  | Base e -> sexp_of_ast e

and sexp_of_ast = function
  | Fn (name, formal, cases) ->
    let sexp_of_case (cond, body) =
      Sexp.List [Array.sexp_of_t (Array.sexp_of_t sexp_of_atom) cond;
                 sexp_of_ast body] in
    Sexp.List (Sexp.Atom "$fn"
               ::(Sexp.Atom (Symbol.to_string name))
               ::(Sexp.Atom (Symbol.to_string formal))
               ::Array.(map sexp_of_case cases |> to_list))
  | App (callee, arg) ->
    Sexp.List [Sexp.Atom "$apply"; sexp_of_ast callee; sexp_of_ast arg]
  | Def (name, expr) ->
    Sexp.List [Sexp.Atom "$def";
               Sexp.Atom (Symbol.to_string name);
               sexp_of_ast expr]
  | Primop ((Expr (opname, _) | Stmt (opname, _) | Ctrl (opname, _)),
            args, [||]) ->
    Sexp.List (Sexp.Atom ("%" ^ opname)
               ::Array.(map sexp_of_ast args |> to_list))
  | Primop ((Expr (opname, _) | Stmt (opname, _) | Ctrl (opname, _)),
            args, conts) ->
    Sexp.List (Sexp.Atom ("%" ^ opname)
               ::(Array.(map sexp_of_ast args |> to_list))
                  @ (Array.(map sexp_of_ast conts |> to_list)))
  | Closure (_, expr) ->
    Sexp.List [Sexp.Atom "Closure"; sexp_of_ast expr]
  | Do stmts ->
    Sexp.List (Sexp.Atom "$do"::Array.(map sexp_of_ast stmts |> to_list))
  | Id name ->
    Symbol.sexp_of_t name
  | Const (Symbol sym) ->
    Sexp.List [Sexp.Atom "$quote"; Symbol.sexp_of_t sym]
  | Const v ->
    sexp_of_value v

(* Exceptions *)

exception CtrError of value * value [@@deriving sexp_of]

exception Not_in_scope of Symbol.t * Scope.Set.t [@@deriving sexp_of]
exception Unbound of Symbol.t [@@deriving sexp_of]
exception Primop_not_found of string [@@deriving sexp_of]
exception Not_a_sf of string [@@deriving sexp_of]
exception Unrecognized_sf of string [@@deriving sexp_of]
exception Invalid_case of value [@@deriving sexp_of]
exception Invalid_fn of value list [@@deriving sexp_of]
exception Invalid_app of value list [@@deriving sexp_of]
exception Invalid_def of value list [@@deriving sexp_of]
exception Invalid_quote of value list [@@deriving sexp_of]

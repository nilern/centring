open Core.Std
module Env = Environment

val sexp_of_bytes : bytes -> Sexp.t

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

module Phase = Int

type ctx = (Scope.Set.t) Phase.Map.t

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (condition * ast) array
        | App of ast * ast
        | Def of int * Symbol.t * ast
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

val atom_ast : atom -> ast

(* Traversals *)

val atom_map : (ast -> ast) -> atom -> atom

val ast_map : (ast -> ast) -> ast -> ast

val walk : (ast -> ast) -> (ast -> ast) -> ast -> ast

val postwalk : (ast -> ast) -> ast -> ast

(* Print *)

val sexp_of_atom : atom -> Sexp.t

val sexp_of_ast : ast -> Sexp.t

val sexp_of_value : value -> Sexp.t

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
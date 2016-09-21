open Core.Std
module Env = Environment

val sexp_of_bytes : bytes -> Sexp.t

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

module Phase = Int

type ctx = (Scope.Set.t) Phase.Map.t

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (condition * ast) array * src_info
        | App of ast * ast * src_info
        | Def of Symbol.t * ast * src_info
        | Primop of primop * ast array * ast array * src_info
        | Closure of env * ast * src_info
        | Do of ast array * src_info
        | Var of Symbol.t * src_info
        | Const of value * src_info

and value = Int of int
          | Bool of bool
          | Char of char
          | Symbol of Symbol.t
          | List of value list
          | Stx of value * ctx * src_info
          | Id of value
          | FnClosure of Symbol.t * Symbol.t * fnbody ref * src_info
          | Record of value * value array
          | Bytes of value * bytes

and primop = Expr of string * (value array -> value)
           | Stmt of string * (value array -> unit)
           | Ctrl of string * (value array -> ast array -> ast)
           | PhExpr of string * (Phase.t -> value array -> value)

and fnbody = Done of ast * (clause * ast * env) Sequence.t
           | Pending of (clause * ast * env) Sequence.t

and atom = Not of ast
         | Base of ast

and clause = atom array

and condition = clause array

(* Accessors *)

val atom_ast : atom -> ast

val ast_pos : ast -> src_info

(* Comparisons *)

val value_equal : value -> value -> bool

val ast_equal : ast -> ast -> bool

(* Syntax Object Scope Operations *)

val get_scopes : Phase.t -> value -> Scope.Set.t

val add_scope : Phase.t -> Scope.t -> value -> value

val flip_scope : Phase.t -> Scope.t -> value -> value

(* Traversals *)

val atom_map : (ast -> ast) -> atom -> atom

val ast_map : (ast -> ast) -> ast -> ast

val walk : (ast -> ast) -> (ast -> ast) -> ast -> ast

val postwalk : (ast -> ast) -> ast -> ast

(* Print *)

val sexp_of_atom : atom -> Sexp.t

val sexp_of_ast : ast -> Sexp.t

val sexp_of_stx : value -> Sexp.t

val sexp_of_value : value -> Sexp.t

(* Exceptions *)

exception Ctr_error of value * value * src_info [@@deriving sexp_of]

exception Primop_not_found of string * src_info [@@deriving sexp_of]
exception Unrecognized_sf of string * src_info [@@deriving sexp_of]
exception Sf_args of string * value list * src_info [@@deriving sexp_of]
exception Invalid_case of value * src_info [@@deriving sexp_of]

exception Not_in_scope of Symbol.t * Scope.Set.t * src_info [@@deriving sexp_of]
exception Unbound of Symbol.t * src_info [@@deriving sexp_of]

exception Type_error of value * value * src_info [@@deriving sexp_of]
exception Uncallable of value * src_info [@@deriving sexp_of]

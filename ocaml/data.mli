open Core.Std
module Env = Environment

val sexp_of_bytes : bytes -> Sexp.t

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (condition * ast) array
        | App of ast * ast
        | Primop of primop * ast array * ast array
        | Closure of env * ast
        | Do of ast array
        | Id of Symbol.t
        | Const of value [@@deriving sexp_of]

and value = Int of int
          | Bool of bool
          | Char of char
          | Symbol of Symbol.t
          | MonoFn of Symbol.t * Symbol.t * ast * env
          (* | PolyFn of Symbol.t * Symbol.t * ast * (ast array * ast * env) Sequence.t *)
          | Record of value * value array
          | Bytes of value * bytes

and primop = Expr of string * (value array -> value)
           | Stmt of string * (value array -> unit)
           | Ctrl of string * (value array -> ast array -> ast)

and atom = Not of ast
         | Base of ast

and clause = atom array

and condition = clause array

type stx = List of stx list * String.Set.t * src_info
         | Atom of value * String.Set.t * src_info

(* Print *)

val sexp_of_ast : ast -> Sexp.t

val sexp_of_primop : primop -> Sexp.t

val value_to_string : value -> string

val sexp_of_value : value -> Sexp.t

val sexp_of_stx : stx -> Sexp.t

open Core.Std
module Env = Environment

val sexp_of_bytes : bytes -> Sexp.t

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (ast * ast) array
        | Primop of primop * ast array * ast array
        | Closure of env * ast
        | Do of ast array
        | Id of Symbol.t
        | Const of value

and value = Int of int
          | Bool of bool
          | Char of char
          | Symbol of Symbol.t
          | FnClosure of Symbol.t * Symbol.t * ast * (ast array * ast * env) Sequence.t
          | Record of value * value array
          | Bytes of value * bytes

and primop = Expr of (value array -> value)
           | Stmt of (value array -> unit)
           | Ctrl of (value array -> ast array -> ast)

type cexp = List of cexp list
          | Atom of value

(* Print *)

val sexp_of_ast : ast -> Sexp.t

val sexp_of_primop : primop -> Sexp.t

val value_to_string : value -> string

val sexp_of_value : value -> Sexp.t

val sexp_of_cexp : cexp -> Sexp.t

open Core.Std
module Env = Environment

let sexp_of_bytes _ = Sexp.Atom "<bytes>"

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
          (* | MonoFn of Symbol.t * Symbol.t * ast * env *)
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

type stx = List of stx list * String.Set.t * src_info
         | Atom of value * String.Set.t * src_info

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

let sexp_of_value v = Sexp.Atom (value_to_string v)

let rec sexp_of_stx = function
  | List (es, _, _) -> Sexp.List (List.map es (fun expr -> sexp_of_stx expr))
  | Atom (e, _, _) -> Sexp.Atom (value_to_string e)
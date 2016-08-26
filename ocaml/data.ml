open Core.Std
module Env = Environment

let sexp_of_bytes _ = Sexp.Atom "<bytes>"

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (ast * ast) array
        | Primop of primop * ast array * ast array
        | Closure of env * ast
        | Do of ast array
        | Id of Symbol.t
        | Const of value [@@deriving sexp_of]

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

type cexp = List of stx list
          | Atom of value

and stx = {expr: cexp; scopes: String.Set.t; src: src_info} [@@deriving sexp_of]

let cexp_to_stx stx cexp = {stx with expr = cexp}

(* Conversions *)

let sexp_of_primop _ = Sexp.Atom "#<primop>"

let rec value_to_string = function
  | Int i -> Int.to_string i
  | Bool b -> if b then "#t" else "#f"
  | Char c -> String.of_char_list ['#'; '\\'; c]
  | Symbol s -> Symbol.to_string s
  | FnClosure (name, _, _, _) -> sprintf "#<Fn %s>" (Symbol.to_string name)
  | Record (t, _) -> value_to_string t
  | Bytes (t, _) -> value_to_string t

let sexp_of_value v = Sexp.Atom (value_to_string v)

let rec sexp_of_cexp = function
  | List es -> Sexp.List (List.map es (fun {expr; _} -> sexp_of_cexp expr))
  | Atom e -> Sexp.Atom (value_to_string e)
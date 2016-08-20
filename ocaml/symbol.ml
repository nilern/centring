open Core.Std

type t = Symbol of string
	   | Gensym of string * int

let of_string s = Symbol s

let gensym_counter = ref 0

let gensym = function
  | Symbol s | Gensym (s, _) ->
    let g = !gensym_counter in
    incr gensym_counter;
    Gensym (s, g)

let sexp_of_t = function
  | Symbol s -> Sexp.Atom s
  | Gensym (s, i) -> Sexp.Atom (s ^ Int.to_string i)

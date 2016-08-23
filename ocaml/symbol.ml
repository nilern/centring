open Core.Std

type t = Symbol of string
	   | Gensym of string * int

let gensym_counter = ref 0

let gensym = function
  | Symbol s | Gensym (s, _) ->
    let g = !gensym_counter in
    incr gensym_counter;
    Gensym (s, g)

let sf_name = function
  | Symbol str -> 
    if String.slice str 0 5 = "##sf#"
    then Some (String.slice str 5 (String.length str))
    else None
  | Gensym _ -> None

let intr_name = function
  | Symbol str -> 
    if String.slice str 0 7 = "##intr#"
    then Some (String.slice str 7 (String.length str))
    else None
  | Gensym _ -> None

(* Conversions *)

let of_string s = Symbol s

let to_string = function
  | Symbol s -> s
  | Gensym (s, i) -> s ^ Int.to_string i

let sexp_of_t = function
  | Symbol s -> Sexp.Atom s
  | Gensym (s, i) -> Sexp.Atom (s ^ Int.to_string i)

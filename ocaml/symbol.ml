open Core.Std

module T = struct
  type t = Symbol of string
  	     | Gensym of string * int

  let of_string s = Symbol s

  let to_string = function
    | Symbol s -> s
    | Gensym (s, i) -> s ^ Int.to_string i

  let t_of_sexp = function
    | Sexp.Atom s -> of_string s
    | Sexp.List _ -> failwith "Cannot parse a Sexp.List as a Symbol.t"

  let sexp_of_t = function
    | Symbol s -> Sexp.Atom s
    | Gensym (s, i) -> Sexp.Atom (s ^ Int.to_string i)

  let compare a b = 
    match (a, b) with
    | (Symbol s1, Symbol s2) -> String.compare s1 s2
    | (Symbol s1, Gensym (s2, i)) ->
      let sres = String.compare s1 s2 in
      if sres = 0
      then -1
      else sres
    | (Gensym (s1, i), Symbol s2) ->
      let sres = String.compare s1 s2 in
      if sres = 0
      then 1
      else sres
    | (Gensym (s1, i1), Gensym (s2, i2)) ->
      let sres = String.compare s1 s2 in
      if sres = 0
      then Int.compare i1 i2
      else sres
end

include T
include Comparable.Make(T)

let gensym_counter = ref 0

let gensym = function
  | Symbol s | Gensym (s, _) ->
    let g = !gensym_counter in
    incr gensym_counter;
    Gensym (s, g)

let sf_name = function
  | Symbol str ->
    let len = String.length str in
    if Int.(len > 5) && String.(slice str 0 5 = "##sf#")
    then Some (String.slice str 5 len)
    else None
  | Gensym _ -> None

let intr_name = function
  | Symbol str -> 
    let len = String.length str in
    if Int.(len > 7) && String.(slice str 0 7 = "##intr#")
    then Some (String.slice str 7 len)
    else None
  | Gensym _ -> None

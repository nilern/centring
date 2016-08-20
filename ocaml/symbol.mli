open Core.Std

type t

val gensym : t -> t

(* Conversions *)

val of_string : string -> t

val to_string : t -> string

val sexp_of_t : t -> Sexp.t

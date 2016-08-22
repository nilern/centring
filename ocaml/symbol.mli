open Core.Std

type t

val gensym : t -> t

val intr_name : t -> string option

(* Conversions *)

val of_string : string -> t

val to_string : t -> string

val sexp_of_t : t -> Sexp.t

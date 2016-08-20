open Core.Std

type t

val of_string : string -> t

val gensym : t -> t

val sexp_of_t : t -> Sexp.t

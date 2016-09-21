open Core.Std

type t

include Comparable.S with type t := t

val gensym : t -> t

val sf_name : t -> string option

val intr_name : t -> string option

(* Conversions *)

val of_string : string -> t

val to_string : t -> string

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

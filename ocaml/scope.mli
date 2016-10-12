open Core.Std

type t = Fn of Symbol.t
       | Use of Symbol.t
       | Intro of Symbol.t
       | Root of int [@@deriving compare, sexp]

include Comparable.S with type t := t

val fresh : t -> t

open Core.Std

module T = struct
	type t = Fn of Symbol.t
	       | Use of Symbol.t
	       | Intro of Symbol.t [@@deriving compare, sexp]
end

include T
include Comparable.Make(T)

let fresh = function
  | Fn s -> Fn (Symbol.gensym s)
  | Use s -> Use (Symbol.gensym s)
  | Intro s -> Intro (Symbol.gensym s)

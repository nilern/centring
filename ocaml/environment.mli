open Core.Std

type ('k, 'v) t

val empty : ('k, 'v) t

val lookup : ('k, 'v) t -> 'k -> 'v

val extend : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t

val set : ('k, 'v) t -> 'k -> 'v -> unit

val sexp_of_t : ('k -> Sexp.t) -> ('v -> Sexp.t) -> ('k, 'v) t -> Sexp.t

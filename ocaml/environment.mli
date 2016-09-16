open Core.Std

type ('k, 'v) t

val empty : unit -> ('k, 'v) t

val push_frame : ('k, 'v) t -> ('k, 'v) t

val merge : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t

val lookup : ('k, 'v) t -> 'k -> 'v Option.t

val def : ('k, 'v) t -> 'k -> 'v -> unit

val sexp_of_t : ('k -> Sexp.t) -> ('v -> Sexp.t) -> ('k, 'v) t -> Sexp.t

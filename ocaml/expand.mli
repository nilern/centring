open Core.Std
open Data

val get_scopes : int -> value -> Scope.Set.t

val resolve : Symbol.t -> Scope.Set.t -> Symbol.t option

val resolve_exn : Symbol.t -> Scope.Set.t -> Symbol.t

val expand : (Symbol.t, value) Env.t -> value -> value

open Core.Std
open Data

type ct_value

val resolve : Symbol.t -> Scope.Set.t option -> Symbol.t

val expand : (Symbol.t, ct_value) Env.t -> value -> value

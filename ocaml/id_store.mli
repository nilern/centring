open Data

val resolve : Symbol.t -> Scope.Set.t -> Symbol.t option

val resolve_exn : Symbol.t -> Scope.Set.t -> src_info -> Symbol.t

val add_binding : Symbol.t -> Scope.Set.t -> Symbol.t -> unit

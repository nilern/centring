open Data

val type_t : value
val int_t : value
val bool_t : value
val char_t : value
val symbol_t : value
val nil_t : value
val pair_t : value
val stx_t : value
val fn_t : value

val tuple_t : value
val macro_t : value

val mmis_sym : Symbol.t

val envs : unit -> ((Symbol.t, value) Env.t * (Symbol.t, value) Env.t)

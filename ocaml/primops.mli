open Core.Std
open Data

val primops : (string, primop) Hashtbl.t

val get : string -> primop option

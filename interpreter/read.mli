open Core.Std
open Data

val read_string : string -> (value, string) Result.t

val read_all : string -> (value, string) Result.t

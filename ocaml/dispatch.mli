open Core.Std
open Data

val dnf : ast -> condition

val fnbody_force : Symbol.t -> src_info -> fnbody ref -> ast

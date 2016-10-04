open Data

val expr : string -> ast array -> src_info -> ast

val stmt : string -> ast array -> src_info -> ast

val ctrl : string -> ast -> ast array -> src_info -> ast

val isa : ast -> ast -> src_info -> ast

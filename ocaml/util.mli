open Core.Std

val seq_of_array : 'a array -> 'a Sequence.t

val seq_of_array_map : ('a -> 'b) -> 'a array -> 'b Sequence.t

val seq_intersection : ('a -> 'a -> bool) -> 'a Sequence.t -> 'a Sequence.t
                       -> 'a Sequence.t

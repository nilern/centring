open Core.Std

let seq_of_array arr =
  let len = Array.length arr in
  let step i =
    if i >= len
    then None
    else Some (arr.(i), i + 1) in
  Sequence.unfold 0 step

let seq_of_array_map f arr =
  let len = Array.length arr in
  let step i =
    if i >= len
    then None
    else Some (f arr.(i), i + 1) in
  Sequence.unfold 0 step

let seq_intersection equal s1 s2 =
  Sequence.(filter ~f:(mem ~equal (memoize s2)) s1)

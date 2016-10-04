open Core.Std
open Data

let expr opname args pos =
  Primop (Primops.get opname |> Option.value_exn, args, [||], pos)

let stmt opname args pos =
  Primop (Primops.get opname |> Option.value_exn, args, [||], pos)

let ctrl opname arg conts pos =
  Primop (Primops.get opname |> Option.value_exn, [|arg|], conts, pos)

let isa v t pos = expr "eq?" [|expr "type" [|v|] pos; t|] pos

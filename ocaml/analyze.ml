open Core.Std
open Data

(* FIXME: exhaustive pattern matches *)

exception Primop_not_found of string

let rec analyze = function
  | Atom Symbol sym -> Id sym
  | Atom v -> Const v
  | List (Atom (Symbol op)::args) when Option.is_some (Symbol.intr_name op) -> 
    analyze_intr op args

and analyze_intr op_name args =
  let open Option in
  match (Symbol.intr_name op_name) >>= Primops.get with
  | Some ((Ctrl _) as op) ->
	Primop (op, [|analyze (List.hd_exn args)|],
	            Some (List.tl_exn args |> Array.of_list |> Array.map analyze))
  | Some op ->
	Primop (op, (args |> Array.of_list |> Array.map analyze), None)
  | None ->
    raise (Primop_not_found (Symbol.to_string op_name))

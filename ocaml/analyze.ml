open Core.Std
open Data

(* FIXME: exhaustive pattern matches *)

exception Primop_not_found of string
exception Not_a_sf of string
exception Unrecognized_sf of string

let rec analyze = function
  | Atom Symbol sym -> Id sym
  | Atom v -> Const v
  | List (Atom (Symbol op)::args) when Option.is_some (Symbol.sf_name op) -> 
    analyze_sf op args
  | List (Atom (Symbol op)::args) when Option.is_some (Symbol.intr_name op) -> 
    analyze_intr op args

and analyze_intr op_name args =
  let open Option in
  match (Symbol.intr_name op_name) >>= Primops.get with
  | Some ((Ctrl _) as op) ->
	Primop (op, [|analyze (List.hd_exn args)|],
	            List.tl_exn args |> Array.of_list_map ~f:analyze)
  | Some op ->
	Primop (op, Array.of_list_map args analyze, [||])
  | None ->
    raise (Primop_not_found (Symbol.to_string op_name))

and analyze_sf sf_name args =
  match (Symbol.sf_name sf_name) with
  | Some "do" ->
    Do (Array.of_list_map args analyze)
  | Some name ->
    raise (Unrecognized_sf name)
  | None ->
    raise (Not_a_sf (Symbol.to_string sf_name))

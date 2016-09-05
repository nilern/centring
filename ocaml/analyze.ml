open Core.Std
open Data
open Dispatch

(* TODO: save src_info in AST *)
(* FIXME: exhaustive pattern matches *)

exception Primop_not_found of string
exception Not_a_sf of string
exception Unrecognized_sf of string
exception Invalid_case of stx [@@deriving sexp_of]
exception Invalid_fn of stx list [@@deriving sexp_of]
exception Invalid_app of stx list [@@deriving sexp_of]
exception Invalid_def of stx list [@@deriving sexp_of]

let rec analyze = function
  | Atom (Symbol sym, _, _) -> Id sym
  | Atom (v, _, _) -> Const v
  | List (Atom (Symbol op, _, _)::args, _, _) 
    when Option.is_some (Symbol.sf_name op) -> 
    analyze_sf op args
  | List (Atom (Symbol op, _, _)::args, _, _) 
    when Option.is_some (Symbol.intr_name op) -> 
    analyze_intr op args
  (* | List (f::args, _, _) ->
    App (analyze f, Array.of_list_map args analyze) *)

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
  | Some "fn" ->
    let analyze_case = function
      | List ([cond; body], _, _) -> (dnf (analyze cond), analyze body)
      | case -> raise (Invalid_case case) in
    (match args with
     | (Atom (Symbol name, _, _))::(Atom (Symbol formal, _, _))::cases ->
       Fn (name, formal, Array.of_list_map cases analyze_case)
     | _ -> raise (Invalid_fn args))
  | Some "apply" ->
    (match args with
     | [callee; arg] ->
       App (analyze callee, analyze arg)
     | _ -> raise (Invalid_app args))
  | Some "def" ->
    (match args with
     | [(Atom (Int i, _, _)); (Atom (Symbol name, _, _)); val_expr] ->
       Def (i, name, analyze val_expr)
     | _ -> raise (Invalid_def args))
  | Some "do" ->
    Do (Array.of_list_map args analyze)
  | Some name ->
    raise (Unrecognized_sf name)
  | None ->
    raise (Not_a_sf (Symbol.to_string sf_name))

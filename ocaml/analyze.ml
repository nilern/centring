open Core.Std
open Data
open Dispatch

(* TODO: save src_info in AST *)
(* FIXME: exhaustive pattern matches *)

let rec analyze = function
  | Stx (List (Stx (Symbol op, _, _)::args), _, _)
    when Option.is_some (Symbol.sf_name op) -> 
    analyze_sf op args
  | Stx (List (Stx (Symbol op, _, _)::args), _, _)
    when Option.is_some (Symbol.intr_name op) -> 
    analyze_intr op args
  | Stx (List (callee::args), _, _) ->
    App (analyze callee, 
         Primop (Option.value_exn (Primops.get "rec"),
                 Array.of_list_map args analyze, [||]))
  | Stx (Symbol sym, ctx, _) as stx ->
    Var (Expand.resolve_exn sym (Expand.get_scopes 0 stx))
  | Stx (v, _, _) -> Const v

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
  match Symbol.sf_name sf_name with
  | Some "fn" ->
    let analyze_case = function
      | Stx (List [cond; body], _, _) -> (dnf (analyze cond), analyze body)
      | case -> raise (Invalid_case case) in
    (match args with
     | (Stx (Symbol nname, nctx, _) as name)
             ::(Stx (Symbol fname, _, _) as formal)
             ::cases ->
       Fn (Expand.resolve_exn nname (Expand.get_scopes 0 name),
           Expand.resolve_exn fname (Expand.get_scopes 0 formal),
           Array.of_list_map cases analyze_case)
     | _ -> raise (Invalid_fn args))
  | Some "apply" ->
    (match args with
     | [callee; arg] ->
       App (analyze callee, analyze arg)
     | _ -> raise (Invalid_app args))
  | Some "def" ->
    (match args with
     | [(Stx (Int 0, _, _)); (Stx (Symbol nsym, _, _)) as name; val_expr] ->
       Def (Expand.resolve_exn nsym (Expand.get_scopes 0 name), analyze val_expr)
     | _ -> raise (Invalid_def args))
  | Some "do" ->
    Do (Array.of_list_map args analyze)
  | Some "quote" ->
    (match args with
     | [(Stx (value, _, _))] -> Const value
     | _ -> raise (Invalid_quote args))
  | Some name ->
    raise (Unrecognized_sf name)
  | None ->
    raise (Not_a_sf (Symbol.to_string sf_name))

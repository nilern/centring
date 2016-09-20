open Core.Std
open Data
open Dispatch

(* TODO: save src_info in AST *)
(* FIXME: exhaustive pattern matches *)

let rec analyze phase = function
  | Stx (List (Stx (Symbol op, _, _)::args), _, pos)
    when Option.is_some (Symbol.sf_name op) ->
    analyze_sf phase op args pos
  | Stx (List (Stx (Symbol op, _, _)::args), _, pos)
    when Option.is_some (Symbol.intr_name op) ->
    analyze_intr phase op args pos
  | Stx (List (callee::args), _, pos) ->
    (* FIXME: output should include `Tuple` *)
    App (analyze phase callee,
         Primop (Option.value_exn (Primops.get "rec"),
                 Array.of_list (Const (Bootstrap.tuple_t, pos)
                                ::List.map args (analyze phase)),
                 [||], pos),
         pos)
  | Stx (Symbol sym, ctx, pos) as stx ->
    Var (Id_store.resolve_exn sym (get_scopes phase stx) pos, pos)
  | Stx (v, _, pos) -> Const (v, pos)

and analyze_intr phase op_name args pos =
  let open Option in
  match (Symbol.intr_name op_name) >>= Primops.get with
  | Some ((Ctrl _) as op) ->
	  Primop (op, [|analyze phase (List.hd_exn args)|],
	          List.tl_exn args |> Array.of_list_map ~f:(analyze phase),
            pos)
  | Some op ->
	  Primop (op, Array.of_list_map args (analyze phase), [||], pos)
  | None ->
    raise (Primop_not_found (Symbol.to_string op_name, pos))

and analyze_sf phase sf_name args pos =
  match Symbol.sf_name sf_name with
  | Some "fn" ->
    let analyze_case = function
      | Stx (List [cond; body], _, _) ->
        (dnf (analyze phase cond), analyze phase body)
      | Stx (_, _, pos) as case ->
        raise (Invalid_case (case, pos)) in
    (match args with
     | (Stx (Symbol nname, nctx, npos) as name)
             ::(Stx (Symbol fname, _, fpos) as formal)
             ::cases ->
       Fn (Id_store.resolve_exn nname (get_scopes phase name) npos,
           Id_store.resolve_exn fname (get_scopes phase formal) fpos,
           Array.of_list_map cases analyze_case,
           pos)
     | _ -> raise (Sf_args ("fn", args, pos)))
  | Some "apply" ->
    (match args with
     | [callee; arg] ->
       App (analyze phase callee, analyze phase arg, pos)
     | _ -> raise (Sf_args ("apply", args, pos)))
  | Some "def" ->
    (match args with
     | [(Stx (Symbol nsym, _, npos)) as name; val_expr] ->
       Def (Id_store.resolve_exn nsym (get_scopes phase name) npos,
            analyze phase val_expr, pos)
     | _ -> raise (Sf_args ("def", args, pos)))
  | Some "do" ->
    Do (Array.of_list_map args (analyze phase), pos)
  | Some "quote" ->
    (match args with
     | [(Stx (value, _, _))] -> Const (value, pos)
     | _ -> raise (Sf_args ("quote", args, pos)))
   | Some "syntax" ->
     (match args with
      | [Stx (_, _, pos) as stx] -> Const (stx, pos)
      | _ -> raise (Sf_args ("syntax", args, pos)))
  | Some name ->
    raise (Unrecognized_sf (name, pos))
  | None ->
    raise (Unrecognized_sf (Symbol.to_string sf_name, pos))

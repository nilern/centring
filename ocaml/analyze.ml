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
    Var (Id_store.resolve_exn sym (get_scopes phase stx), pos)
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
    raise (Primop_not_found (Symbol.to_string op_name))

and analyze_sf phase sf_name args pos =
  match Symbol.sf_name sf_name with
  | Some "fn" ->
    let analyze_case = function
      | Stx (List [cond; body], _, _) ->
        (dnf (analyze phase cond), analyze phase body)
      | case ->
        raise (Invalid_case case) in
    (match args with
     | (Stx (Symbol nname, nctx, _) as name)
             ::(Stx (Symbol fname, _, _) as formal)
             ::cases ->
       Fn (Id_store.resolve_exn nname (get_scopes phase name),
           Id_store.resolve_exn fname (get_scopes phase formal),
           Array.of_list_map cases analyze_case,
           pos)
     | _ -> raise (Invalid_fn args))
  | Some "apply" ->
    (match args with
     | [callee; arg] ->
       App (analyze phase callee, analyze phase arg, pos)
     | _ -> raise (Invalid_app args))
  | Some "def" ->
    (match args with
     | [(Stx (Symbol nsym, _, _)) as name; val_expr] ->
       Def (Id_store.resolve_exn nsym (get_scopes phase name),
            analyze phase val_expr, pos)
     | _ -> raise (Invalid_def args))
  | Some "do" ->
    Do (Array.of_list_map args (analyze phase), pos)
  | Some "quote" ->
    (match args with
     | [(Stx (value, _, _))] -> Const (value, pos)
     | _ -> raise (Invalid_quote args))
   | Some "syntax" ->
     (match args with
      | [Stx (_, _, pos) as stx] -> Const (stx, pos)
      | _ -> raise (Invalid_syntax args))
  | Some name ->
    raise (Unrecognized_sf name)
  | None ->
    raise (Not_a_sf (Symbol.to_string sf_name))

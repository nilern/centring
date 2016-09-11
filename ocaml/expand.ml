open Core.Std
open Data

type ct_value = Id of value

let add_to_scopes scope = function
  | Some scopes -> Set.add scopes scope
  | None -> Scope.Set.singleton scope

let add_to_ctx scope ctx = Map.update ctx 0 (add_to_scopes scope)

let rec add_scope scope stx =
  match stx with
  | Stx (List stxen, ctx, pos) ->
    Stx (List (List.map stxen (add_scope scope)), add_to_ctx scope ctx, pos)
  | Stx (value, ctx, pos) ->
    Stx (value, add_to_ctx scope ctx, pos)

let bindings = Hashtbl.create ~hashable:Hashtbl.Poly.hashable ()

let add_binding id scopes new_sym =
  match Hashtbl.find bindings id with
  | Some scp_tbl ->
    Hashtbl.set scp_tbl ~key:scopes ~data:new_sym
  | None ->
    let scp_tbl = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
	Hashtbl.set scp_tbl ~key:scopes ~data:new_sym;
	Hashtbl.set bindings ~key:id ~data:scp_tbl

let resolve id scopes =
  let open Option in
  let scopes = value scopes ~default:Scope.Set.empty in
  let step ~key ~data res =
    if Set.subset key scopes
    then match res with
         | Some (rscopes, _) when Set.subset key rscopes -> res
         | _ -> Some (key, data)
    else res in
  match Hashtbl.find bindings id >>= Hashtbl.fold ~init:None ~f:step with
  | Some (_, sym) -> sym
  | None -> raise (Not_in_scope (id, scopes))

let rec expand env stx =
  match stx with
  | Stx (List (Stx (Symbol op, _, _)::args), _, _)
    when Option.is_some (Symbol.sf_name op) ->
    expand_sf env op stx
  | Stx (List stxen, ctx, pos) ->
    Stx (List (List.map stxen (expand env)), ctx, pos)
  | Stx (Symbol id, ctx, pos) ->
  	(match Env.lookup env (resolve id (Map.find ctx 0)) with
  	 | Some (Id stx') -> stx'
  	 | None -> raise (Unbound id))
  | _ -> stx

and expand_sf env op stx =
  match Symbol.sf_name op with
  | Some "quote" ->
  	(match stx with
     | Stx (List [_; quoted], _, _) -> stx
     | Stx (List (_::quoted), _, _) -> raise (Invalid_quote quoted))
  | Some "fn" ->
  	(match stx with
     | Stx (List (fnsym
     	          ::(Stx (Symbol nsym, _, _) as name)
     	          ::(Stx (Symbol fsym, fctx, _) as formal)
     	          ::cases), ctx, pos) ->
       let new_scope = Scope.fresh (Scope.Fn nsym) in
       let formal' = add_scope new_scope formal in

       let new_sym = Symbol.gensym fsym in
       let env' = Env.merge env (Env.empty ()) in
       let scopes' = add_to_scopes new_scope (Map.find fctx 0) in
       add_binding fsym scopes' new_sym;
       Env.def env' new_sym (Id formal');

       let cases' =
         List.map cases (Fn.compose (expand env') (add_scope new_scope)) in
       Stx (List (fnsym::name::formal'::cases'), ctx, pos))
  | Some "def" ->
    (match stx with
     | Stx (List [defsym; Stx (Int 0, _, _) as ph;
                  Stx (Symbol nsym, _, _) as name; val_expr], ctx, pos) ->
       let scopes = (Option.value (Map.find ctx 0) ~default:Scope.Set.empty) in
       let name' = name |> expand env in
       let new_sym = Symbol.gensym nsym in

       add_binding nsym scopes new_sym;
       Env.def env new_sym (Id name');

       Stx (List [defsym; ph; name'; expand env val_expr], ctx, pos)
     | Stx (List (_::args), _, _) -> raise (Invalid_def args))
  | Some "do" | Some "apply" ->
  	(match stx with
  	 | Stx (List (opsym::stmts), ctx, pos) ->
  	   Stx (List (opsym::(List.map stmts (expand env))), ctx, pos))
  | Some name ->
    raise (Unrecognized_sf name)
  | None ->
    raise (Not_a_sf (Symbol.to_string op))

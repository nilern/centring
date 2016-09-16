open Core.Std
open Data

let get_scopes phase (Stx (_, ctx, _)) =
  Option.value (Map.find ctx phase) ~default:Scope.Set.empty

let rec add_scope phase scope stx =
  let add_to_scopes scope = function
    | Some scopes -> Set.add scopes scope
    | None -> Scope.Set.singleton scope in
  let add_to_ctx scope ctx =
    Map.update ctx phase (add_to_scopes scope) in
  match stx with
  | Stx (List stxen, ctx, pos) ->
    Stx (List (List.map stxen (add_scope phase scope)),
         add_to_ctx scope ctx, pos)
  | Stx (value, ctx, pos) ->
    Stx (value, add_to_ctx scope ctx, pos)

let rec defnames stx =
  let open Set in
  match stx with
  | Stx (List (Stx (Symbol op , _, _)::_), _, _)
    when Symbol.sf_name op = Some "fn" ->
    Poly.empty
  | Stx (List [Stx (Symbol op , _, _); Stx (Symbol _, _, _) as name; expr],
         _, _) when Symbol.sf_name op = Some "def" ->
    union (Poly.singleton name) (defnames expr)
  | Stx (List stxen, ctx, pos) ->
    List.fold stxen ~init:Poly.empty
                    ~f:(fun acc stx -> union acc (defnames stx))
  | Stx _ as stx ->
    Poly.empty

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
  let step ~key ~data res =
    if Set.subset key scopes
    then match res with
         | Some (rscopes, _) when Set.subset key rscopes -> res
         | _ -> Some (key, data)
    else res in
  Hashtbl.find bindings id
  >>= (Hashtbl.fold ~init:None ~f:step)
  >>| (fun (_, sym) -> sym)

let resolve_exn id scopes =
  match resolve id scopes with
  | Some v -> v
  | None -> raise (Not_in_scope (id, scopes))

let rec expand phase env stx =
  let open Option in
  match stx with
  | Stx (List (Stx (Symbol op, _, _)::args), _, _)
    when is_some (Symbol.sf_name op) ->
    expand_sf phase env op stx
  | Stx (List stxen, ctx, pos) ->
    Stx (List (List.map stxen (expand phase env)), ctx, pos)
  | Stx (Symbol id, ctx, pos) as stx ->
    let scopes = (get_scopes phase stx) in
  	(match resolve id scopes >>= (Env.lookup env) with
  	 | Some (Id stx') -> stx'
  	 | None -> stx)
  | _ -> stx

and expand_sf phase env op stx =
  match Symbol.sf_name op with
  | Some "fn" -> expand_fn phase env stx
  | Some "apply" -> expand_triv_sf phase env stx
  | Some "do" -> expand_triv_sf phase env stx
  | Some "def" -> expand_def phase env stx
  | Some "quote" -> expand_quote stx

  | Some name -> raise (Unrecognized_sf name)
  | None -> raise (Not_a_sf (Symbol.to_string op))

and expand_fn phase env = function
  | Stx (List (fnsym
               ::(Stx (Symbol nsym, _, _) as name)
               ::(Stx (Symbol _, _, _) as formal)
                ::cases), ctx, pos) ->
    let env' = Env.push_frame env in
    let new_scope = Scope.fresh (Scope.Fn nsym) in

    let name' = add_scope phase new_scope name in
    let formal' = add_scope phase new_scope formal in
    let cases' = List.map cases (add_scope phase new_scope) in

    let ivars = Stx (List cases', ctx, pos)
      |> expand phase (Env.push_frame env) |> defnames 
      |> Fn.flip Set.add name' |> Fn.flip Set.add formal' in
    Set.iter ivars (function
      | Stx (Symbol ivsym, _, _) as ivstx ->
        let ivsym' = Symbol.gensym ivsym in
        add_binding ivsym (get_scopes phase ivstx) ivsym';
        Env.def env' ivsym' (Id ivstx));

    let cases'' = List.map cases' (expand phase env') in
    Stx (List (fnsym::name'::formal'::cases''), ctx, pos)

and expand_triv_sf phase env = function
  | Stx (List (opsym::stmts), ctx, pos) ->
    Stx (List (opsym::(List.map stmts (expand phase env))), ctx, pos)

and expand_def phase env = function
  | Stx (List [defsym;
               Stx (Symbol nsym, _, _) as name; val_expr], ctx, pos) as stx ->
    let new_sym = Symbol.gensym nsym in
    add_binding nsym (get_scopes phase stx) new_sym;
    Env.def env new_sym (Id name);

    Stx (List [defsym; name; expand phase env val_expr], ctx, pos)
  | Stx (List (_::args), _, _) -> raise (Invalid_def args)

and expand_quote = function
  | Stx (List [_; quoted], _, _) as stx -> stx
  | Stx (List (_::quoted), _, _) -> raise (Invalid_quote quoted)
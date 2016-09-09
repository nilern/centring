open Core.Std
open Data

type ct_value = Id of value

let bindings = Hashtbl.create ~hashable:Hashtbl.Poly.hashable ()

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

let resolve id scopes =
  let scopes = Option.value scopes ~default:Scope.Set.empty in
  match Hashtbl.find bindings (id, scopes) with
  | Some sym -> sym
  | None -> raise (Not_in_scope (id, scopes))

let rec expand env stx =
  match stx with
  | Stx (List (Stx (Symbol op, _, _)::args), _, _)
    when Option.is_some (Symbol.sf_name op) ->
    expand_sf env op stx
  | Stx (Symbol id, ctx, pos) ->
  	(match Env.lookup env (resolve id (Map.find ctx 0)) with
  	 | Some (Id stx') -> stx'
  	 | None -> raise (Unbound id))
  | _ -> stx

and expand_sf env op stx =
  match Symbol.sf_name op with
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
       Hashtbl.set bindings
                   ~key:(fsym, add_to_scopes new_scope (Map.find fctx 0))
                   ~data:new_sym;
       Env.def env' new_sym (Id formal');

       let cases' =
         List.map cases (Fn.compose (expand env') (add_scope new_scope)) in
       Stx (List (fnsym::name::formal'::cases'), ctx, pos))
  | Some "do" ->
  	(match stx with
  	 | Stx (List (dosym::stmts), ctx, pos) ->
  	   Stx (List (dosym::(List.map stmts (expand env))), ctx, pos))
  | Some "quote" | Some "syntax" ->
  	(match stx with
     | Stx (List [_; quoted], _, _) -> stx
     | Stx (List (_::quoted), _, _) -> raise (Invalid_quote quoted))

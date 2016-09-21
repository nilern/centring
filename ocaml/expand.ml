open Core.Std
open Data
open Id_store

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

let rec expand phase env stx =
  let open Option in
  match stx with
  | Stx (List (Stx (Symbol op, _, _)::args as stxen), ctx, pos) ->
    if is_some (Symbol.sf_name op)
    then expand_sf phase env op stx
    else let scopes = (get_scopes phase stx) in
         (match resolve op scopes >>= (Env.lookup env) with
          | Some (Record (t, [|mac|])) when t == Bootstrap.macro_t ->
            let (scope_u, scope_i) = match mac with
            | FnClosure (macname, _, _, _) ->
              (Scope.fresh (Scope.Use macname),
               Scope.fresh (Scope.Intro macname))
            | _ ->
             (Scope.fresh (Scope.Use (Symbol.of_string "macro")),
              Scope.fresh (Scope.Intro (Symbol.of_string "macro"))) in
            let stx =
              stx |> add_scope phase scope_u |> add_scope phase scope_i in
            let mac_res =
              Cek.interpret phase env (App (Const (mac, pos),
                                            Const (stx, pos), pos)) in
            expand phase env (flip_scope phase scope_i mac_res)
          | Some _ | None ->
            Stx (List (List.map stxen (expand phase env)), ctx, pos))
  | Stx (List stxen, ctx, pos) ->
    Stx (List (List.map stxen (expand phase env)), ctx, pos)
  | Stx (Symbol id, ctx, pos) as stx ->
    let scopes = (get_scopes phase stx) in
  	(match resolve id scopes >>= (Env.lookup env) with
  	 | Some (Id stx') -> stx'
     | Some _ | None -> stx)
  | _ -> stx

and expand_sf phase env op stx =
  match Symbol.sf_name op with
  | Some "fn" -> expand_fn phase env stx
  | Some "apply" -> expand_triv_sf phase env stx
  | Some "do" -> expand_triv_sf phase env stx
  | Some "def" -> expand_def phase env stx
  | Some "meta" -> let (stx', _) = expand_meta phase env stx in stx'
  | Some "quote" | Some "syntax" -> expand_quote stx

  | Some name ->
    (match stx with
     | Stx (_, _ , pos) -> raise (Unrecognized_sf (name, pos))
     | _ -> raise (Unrecognized_sf (name, {filename = "";
                                           index = 0; row = 0; col = 0})))
  | None -> raise (Unrecognized_sf ("", {filename = "";
                                         index = 0; row = 0; col = 0}))

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
  | Stx (List [defsym; Stx (Symbol nsym, _, _) as name; expr],
         ctx, pos) as stx ->
    let new_sym = Symbol.gensym nsym in
    add_binding nsym (get_scopes phase stx) new_sym;
    (match expr with
     | Stx (List [Stx (Symbol op, _, _); _], _, _)
       when Symbol.sf_name op = Some "meta" ->
       let (stx', v) = expand_meta phase env expr in
       Env.def env new_sym v;
       stx'
     | _ ->
       Env.def env new_sym (Id name);
       Stx (List [defsym; name; expand phase env expr], ctx, pos))
  | Stx (List (_::args), _, pos) ->
    raise (Sf_args ("def", args, pos))

and expand_meta phase env = function
  | Stx (List [op; expr], ctx, pos) ->
    let phase' = phase + 1 in
    (Stx (List [Stx (Symbol (Symbol.of_string "##sf#do"), ctx, pos)], ctx, pos),
     expr |> expand phase' env |> Analyze.analyze phase' |> Cek.interpret phase' env)

and expand_quote = function
  | Stx (List [_; quoted], _, _) as stx -> stx
  | Stx (List (_::args), _, pos) -> raise (Sf_args ("quote", args, pos))

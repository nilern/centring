open Core.Std
open Data

let bindings = Hashtbl.create ~hashable:Hashtbl.Poly.hashable ()

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

let resolve_exn id scopes pos =
  match resolve id scopes with
  | Some v -> v
  | None -> raise (Not_in_scope (id, scopes, pos))

let add_binding id scopes new_sym =
  match Hashtbl.find bindings id with
  | Some scp_tbl ->
    Hashtbl.set scp_tbl ~key:scopes ~data:new_sym
  | None ->
    let scp_tbl = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
	  Hashtbl.set scp_tbl ~key:scopes ~data:new_sym;
	  Hashtbl.set bindings ~key:id ~data:scp_tbl

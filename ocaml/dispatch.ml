open Core.Std
open Data

let bior = Primops.get "bior" |> Option.value_exn
let band = Primops.get "band" |> Option.value_exn
let bnot = Primops.get "bnot" |> Option.value_exn
let brf = Primops.get "brf" |> Option.value_exn
let err = Primops.get "err" |> Option.value_exn

let wrap_connectives node = [|[|Base node|]|]

let or_default = (wrap_connectives (Const (Bool false)))
let and_default = (wrap_connectives (Const (Bool true)))

let negate expr = Primop (bnot, [|expr|], [||])

(* Like Array.fold, but only uses default when array is non-empty: *)
let fold_conds default f = function
  | [||] -> default
  | [|cond|] -> cond
  | conds ->
    let len = Array.length conds in
    let rec loop i acc =
      let i' = i + 1 in
        if i' >= len
        then acc
        else loop i' (f acc conds.(i')) in
    loop 0 conds.(0)

let rec dnf = function
  | Primop (Expr ("bior", _), args, _) -> (* Flatten Ors *)
    fold_conds or_default Array.append (Array.map dnf args)
  | Primop (Expr ("band", _), args, _) -> (* Distribute And over Ors *)
    let combine acc v =
      let acc_len = Array.length acc in
      let v_len = Array.length v in
      let res = Array.create (acc_len * v_len) [||] in
      for i = 0 to (acc_len - 1) do
        let l = acc.(i) in
        for j = 0 to (v_len - 1) do
          let r = v.(j) in
          res.(i*v_len + j) <- Array.append l r
        done
      done;
      res in
    fold_conds and_default combine (Array.map dnf args)
  | Primop (Expr ("bnot", _), [|arg|], _) as ast ->
    (match arg with
     | Primop (Expr ("bior", _), args, _) -> (* De Morgan *)
       dnf (Primop (band, Array.map negate args, [||]))
     | Primop (Expr ("band", _), args, _) -> (* De Morgan *)
       dnf (Primop (bior, Array.map negate args, [||]))
     | Primop (Expr ("bnot", _), [|inner_arg|], _) -> (* Double negation *)
       dnf inner_arg
     | _ -> [|[|Not arg|]|])
  | ast -> wrap_connectives ast

let atom_closures methods =
  let open Sequence in
  methods >>= (fun (clause, body, env) ->
                Util.seq_of_array_map (fun e -> (atom_ast e, env)) clause)

let target_closures expr methods' exprs =
  Util.seq_intersection (Sequence.filter ~f:(fun (e, _) -> e <> expr) exprs)
                        (atom_closures methods')

let target_methods expr truthy methods =
  let atom_passes atom =
    match (truthy, atom) with
    | (true, Base _) -> true
    | (true, Not e) -> e <> expr
    | (false, Base e) -> e <> expr
    | (false, Not _) -> true in
  let method_passes (clause, _, _) = Array.for_all clause atom_passes in
  Sequence.filter methods method_passes

(* OPTIMIZE: implement heuristics *)
let pick_closure exprs methods = Sequence.hd exprs

(* TODO: deal with overrides ("min<=_method") *)
(* TODO: error messages *)
let compute_target methods =
  match Sequence.bounded_length methods 1 with
  | `Is 0 ->
    Primop (err, [|Const (Symbol (Symbol.of_string "NoMethodError"));
                   Const (Bool false)|], [||])
  | `Is 1 ->
    let (_, body, env) = Sequence.hd_exn methods in
    Closure (env, body)
  | `Greater ->
    Primop (err, [|Const (Symbol (Symbol.of_string "AmbiguousMethodError"));
                   Const (Bool false)|], [||])

(* OPTIMIZE: memoization *)
let build_dag methods =
  let rec build_sub_dag methods exprs =
    match pick_closure exprs methods with
    | Some (expr, env) ->
      Primop (brf, [|Closure (env, expr)|],
                   [|build_sub_dag_ass expr true methods exprs;
                     build_sub_dag_ass expr false methods exprs|])
    | None -> compute_target methods
  and build_sub_dag_ass expr truthy methods exprs = 
    let methods' = target_methods expr truthy methods in
    let exprs' = target_closures expr methods' exprs in
    build_sub_dag methods' exprs' in
  build_sub_dag methods (atom_closures methods)

let fnbody_force = function
  | {contents = Done (body, _)} -> body
  | {contents = Pending methods} as r ->
    let body = build_dag methods in
    r := Done (body, methods);
    body
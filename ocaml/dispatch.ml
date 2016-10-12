open Core.Std
open Data

let bior = Primops.get "bior" |> Option.value_exn
let band = Primops.get "band" |> Option.value_exn
let bnot = Primops.get "bnot" |> Option.value_exn
let brf = Primops.get "brf" |> Option.value_exn
let err = Primops.get "err" |> Option.value_exn

let nopos = {filename = ""; index = 0; row = 0; col = 0}

let wrap_connectives node = [|[|Base node|]|]

let or_default =
  (wrap_connectives (Const (Bool false, nopos)))
let and_default =
  (wrap_connectives (Const (Bool true, nopos)))

let negate expr = Primop (bnot, [|expr|], [||], ast_pos expr)

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
  | Primop (Expr ("bior", _), args, _, _) -> (* Flatten Ors *)
    fold_conds or_default Array.append (Array.map dnf args)
  | Primop (Expr ("band", _), args, _, _) -> (* Distribute And over Ors *)
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
  | Primop (Expr ("bnot", _), [|arg|], _, _) as ast ->
    (match arg with
     | Primop (Expr ("bior", _), args, _, pos) -> (* De Morgan *)
       dnf (Primop (band, Array.map negate args, [||], pos))
     | Primop (Expr ("band", _), args, _, pos) -> (* De Morgan *)
       dnf (Primop (bior, Array.map negate args, [||], pos))
     | Primop (Expr ("bnot", _), [|inner_arg|], _, _) -> (* Double negation *)
       dnf inner_arg
     | _ -> [|[|Not arg|]|])
  | ast -> wrap_connectives ast

let atom_closures methods =
  let open Sequence in
  methods >>= (fun (clause, body, env) ->
                Util.seq_of_array_map (fun e -> (atom_ast e, env)) clause)

let atom_closure_equal (a1, env1) (a2, env2) = ast_equal a1 a2 && env1 == env2

let target_closures expr methods' exprs =
  Util.seq_intersection atom_closure_equal
    (Sequence.filter ~f:(fun (e, _) -> not (ast_equal e expr)) exprs)
    (atom_closures methods')

let target_methods expr truthy methods =
  let atom_passes atom =
    match (truthy, atom) with
    | (true, Base _) -> true
    | (true, Not e) -> not (ast_equal e expr)
    | (false, Base e) -> not (ast_equal e expr)
    | (false, Not _) -> true in
  let method_passes (clause, _, _) = Array.for_all clause atom_passes in
  Sequence.filter methods method_passes

(* OPTIMIZE: implement heuristics *)
let pick_closure exprs methods = Sequence.hd exprs

let emit_override_expr pos meth1 meth2 =
  let tautology cond =
    Array.for_all cond ~f:(function
      | Base Const (Bool false, _) -> false
      | Base Const (v, _) -> true
      | Not Const (Bool false, _) -> true
      | Not Const (c, _) -> false
      | _ -> false) in
  match (meth1, meth2) with
  | ((_, body1, env1), (_, body2, env2))
    when body1 == body2 && env1 == env2 -> (* methods execute identically? *)
    Const (Int 0, pos)
  | ((c1, _, _), (c2, _, _)) ->
    (* If the condition of one is a tautology, pick the other one: *)
    (match (tautology c1, tautology c2) with
     | (false, false) -> Const (Int 1, pos)
     | (false, true) -> Const (Int 0, pos)
     | (true, false) -> Const (Int 2, pos)
     | (true, true) -> Const (Int 1, pos))

let disambiguate name pos methods =
  let methods_arr = Sequence.to_array methods in
  let (_, _, env0) = methods_arr.(0) in
  let method_cmp = (* ($fn method-cmp args
                       ((%band (: args Tuple) (%ieq? (%rlen args) 2))
                        (%switch (%rref args 0)
                          (%switch (%rref args 1) ...)
                          ...))) *)
    let args_sym = "args" |> Symbol.of_string |> Symbol.gensym in
    let args_var = Var (args_sym, pos) in
    let override_switch i meth =
      Ast.ctrl "switch" (Ast.expr "rref" [|args_var; Const (Int 1, pos)|] pos)
        (Array.map ~f:(emit_override_expr pos meth) methods_arr)
        pos in
    Fn ("method-cmp" |> Symbol.of_string |> Symbol.gensym,
        args_sym,
        [|([|[|Base (Ast.isa args_var (Const (Bootstrap.tuple_t, pos)) pos);
               Base (Ast.expr "ieq?" [|Ast.expr "rlen" [|args_var|] pos;
                                       Const (Int 2, pos)|] pos)|]|],
           Ast.ctrl "switch"
             (Ast.expr "rref" [|args_var; Const (Int 0, pos)|] pos)
             (Array.mapi ~f:override_switch methods_arr) pos)|],
        pos) in
  let mmis_sym = "mmis" |> Symbol.of_string |> Symbol.gensym in
  let mmis_var = Var (mmis_sym, pos) in
  let mmis = (* (max-method-indices ,method_cmp ,(length methods)) *)
    App (Closure(env0, Var (Id_store.resolve_exn Bootstrap.mmis_sym
                              (Scope.Set.singleton (Scope.Root 0)) pos, pos),
                 pos),
         Ast.expr "rec" [|Const (Bootstrap.tuple_t, pos);
                          method_cmp;
                          Const (Int (Sequence.length methods), pos)|] pos,
         pos) in
  let type_test = (* (%brf (: (%cdr ,mmis_sym) List.Empty)) *)
    Ast.isa (Ast.expr "cdr" [|mmis_var|] pos)
            (Const (Bootstrap.nil_t, pos)) pos in
  let method_switch =
    Ast.ctrl "switch" (Ast.expr "car" [|mmis_var|] pos)
      (Array.map ~f:(fun (_, body, env) -> Closure (env, body, ast_pos body))
                 methods_arr)
      pos in
  let amb_err = (* (%err 'AmbiguousMethodError ,name) *)
    Ast.stmt "err"
      [|Const (Symbol (Symbol.of_string "AmbiguousMethodError"), pos);
        Const (Stx (Symbol name, Phase.Map.empty, pos), pos)|]
      pos in
  Do ([|(* ($def ,mmis_sym ,mmis) *)
        Def (mmis_sym, mmis, pos);
        (* (%brf ,type_test ,method_switch ,amb_err) *)
        Ast.ctrl "brf" type_test [|method_switch; amb_err|] pos|],
      pos)

(* TODO: error messages *)
let compute_target name pos methods =
  match Sequence.bounded_length methods 1 with
  | `Is 0 ->
    Primop (err, [|Const (Symbol (Symbol.of_string "NoMethodError"), pos);
                   Const (Stx (Symbol name, Phase.Map.empty, pos), pos)|],
                   [||], pos)
  | `Is 1 ->
    let (_, body, env) = Sequence.hd_exn methods in
    Closure (env, body, ast_pos body)
  | `Greater ->
    disambiguate name pos methods

(* OPTIMIZE: memoization *)
let build_dag fname fpos methods =
  let rec build_sub_dag methods exprs =
    match pick_closure exprs methods with
    | Some (expr, env) ->
      let pos = ast_pos expr in
      Primop (brf, [|Closure (env, expr, pos)|],
                   [|build_sub_dag_ass expr true methods exprs;
                     build_sub_dag_ass expr false methods exprs|],
                   pos)
    | None -> compute_target fname fpos methods
  and build_sub_dag_ass expr truthy methods exprs =
    let methods' = target_methods expr truthy methods in
    let exprs' = target_closures expr methods' exprs in
    build_sub_dag methods' exprs' in
  build_sub_dag methods (atom_closures methods)

let fnbody_force name pos = function
  | {contents = Done (body, _)} -> body
  | {contents = Pending methods} as r ->
    let body = build_dag name pos methods in
    r := Done (body, methods);
    body

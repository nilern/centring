open Data
open Core.Std
module Env = Environment

(* FIXME: exhaustive matches *)

type cont = Fn of ast * env * cont * src_info
          | Arg of value * env * cont * src_info
          | Def of Symbol.t * env * cont * src_info
          | Primop of primop * value list * ast array * int * ast array
                    * env * cont * src_info
          | Do of ast array * int * env * cont * src_info
          | Halt

let interpret phase env ast =
  let rec eval ctrl env k =
    match ctrl with
    | Data.Fn (name, formal,
               [|([|[|Base (Const (Bool true, _))|] as clause|], body)|], pos) ->
      (* TODO: move this optimization to analysis phase *)
      let payload = Done (Closure (env, body, pos),
                          Sequence.singleton (clause, body, env)) in
      continue (FnClosure (name, formal, ref payload, pos)) k
    | Data.Fn (name, formal, methods, pos) ->
      let open Sequence in
      let split_close (clauses, body) =
        Util.seq_of_array_map (fun clause -> (clause, body, env)) clauses in
      let payload =
        Pending ((Util.seq_of_array methods) >>= split_close) in
      continue (FnClosure (name, formal, ref payload, pos)) k
    | Data.App (f, args, pos) ->
      eval f env (Fn (args, env, k, pos))
    | Data.Def (name, expr, pos) ->
      eval expr env (Def (name, env, k, pos))
    | Data.Primop (op, [||], conts, _) ->
      apply_primop op [||] conts env k
    | Data.Primop (op, args, conts, pos) ->
      eval args.(0) env (Primop (op, [], args, 0, conts, env, k, pos))
    | Data.Closure (env', ast', _) ->
      eval ast' (Env.merge env env') k
    | Data.Do ([||], _) ->
      continue (Record (Bootstrap.tuple_t, [||])) k
    | Data.Do (stmts, pos) ->
      eval stmts.(0) env (Do (stmts, 0, env, k, pos))
    | Var (name, pos) ->
      (match Env.lookup env name with
       | Some v -> continue v k
       | None ->
         raise (Ctr_error (Symbol (Symbol.of_string "UnboundVariable"),
                           Symbol name, pos)))
    | Const (v, _) ->
      continue v k

  and continue v k =
    match k with
    | Fn (arg, env, k', pos) ->
      eval arg env (Arg (v, env, k', pos))
    | Arg (f, env, k', pos) ->
      apply f v k' pos
    | Def (name, env, k', pos) ->
      Env.def env name v;
      continue (Record (Bootstrap.tuple_t, [||])) k'
    | Primop (op, vals, args, i, conts, env, k', pos) ->
      let i' = i + 1 in
      let vals' = v::vals in
      if i' = Array.length args
      then apply_primop op (Array.of_list_rev vals') conts env k'
      else eval args.(i') env (Primop (op, vals', args, i', conts, env, k', pos))
    | Do (stmts, i, env, k', pos) ->
      let i' = i + 1 in
      if i' = Array.length stmts
      then continue v k'
      else eval stmts.(i') env (Do (stmts, i', env, k', pos))
    | Halt -> v

  and apply f arg k pos =
    match f with
    | FnClosure (fname, formal, payload, pos) ->
      let env = Env.empty () in
      Env.def env fname f;
      Env.def env formal arg;
      eval (Dispatch.fnbody_force fname pos payload) env k
    | _ ->
      raise (Uncallable (f, pos))

  and apply_primop op vals conts env k =
    match op with
    | Expr (_, f) -> continue (f vals) k
    | Stmt (_, f) -> f vals; continue (Record (Bootstrap.tuple_t, [||])) k
    | Ctrl (_, f) -> eval (f vals conts) env k
    | PhExpr (_, f) -> continue (f phase vals) k in
  eval ast env Halt

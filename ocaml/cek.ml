open Data
open Core.Std
module Env = Environment

(* FIXME: exhaustive matches *)

type cont = Fn of ast * env * cont
          | Arg of value * env * cont
          | Def of Symbol.t * env * cont
          | Primop of primop * value list * ast array * int * ast array
                    * env * cont
          | Do of ast array * int * env * cont
          | Halt

let interpret ast = 
  let rec eval ctrl env k =
    match ctrl with
    | Data.Fn (name, formal, 
               [|([|[|Base (Const (Bool true))|] as clause|], body)|]) ->
      (* TODO: move this optimization to analysis phase *)
      let payload = Done (Closure (env, body), 
                          Sequence.singleton (clause, body, env)) in
      continue (FnClosure (name, formal, ref payload)) k
    | Data.Fn (name, formal, methods) ->
      let open Sequence in
      let split_close (clauses, body) =
        Util.seq_of_array_map (fun clause -> (clause, body, env)) clauses in
      let payload = 
        Pending ((Util.seq_of_array methods) >>= split_close) in
      continue (FnClosure (name, formal, ref payload)) k
    | Data.App (f, args) ->
      eval f env (Fn (args, env, k))
    | Data.Def (name, expr) ->
      eval expr env (Def (name, env, k))
    | Data.Primop (op, [||], conts) ->
      apply_primop op [||] conts env k
    | Data.Primop (op, args, conts) ->
      eval args.(0) env (Primop (op, [], args, 0, conts, env, k))
    | Data.Closure (env', ast') ->
      eval ast' (Env.merge env env') k
    | Data.Do [||] ->
      (* FIXME: should continue with empty tuple: *)
      continue (Bool false) k
    | Data.Do stmts ->
      eval stmts.(0) env (Do (stmts, 0, env, k))
    | Var name ->
      (match Env.lookup env name with
       | Some v -> continue v k
       | None -> 
         raise (CtrError (Symbol (Symbol.of_string "UnboundVariable"),
                          Symbol name)))
    | Const v ->
      continue v k

  and continue v k =
    match k with
    | Fn (arg, env, k') ->
      eval arg env (Arg (v, env, k'))
    | Arg (f, env, k') ->
      apply f v k'
    | Def (name, env, k') ->
      Env.def env name v;
      (* FIXME: should continue with empty tuple: *)
      continue (Bool false) k'
    | Primop (op, vals, args, i, conts, env, k') ->
      let i' = i + 1 in
      let vals' = v::vals in
      if i' = Array.length args
      then apply_primop op (Array.of_list_rev vals') conts env k'
      else eval args.(i') env (Primop (op, vals', args, i', conts, env, k'))
    | Do (stmts, i, env, k') ->
      let i' = i + 1 in
      if i' = Array.length stmts
      then continue v k'
      else eval stmts.(i') env (Do (stmts, i', env, k'))
    | Halt -> v

  and apply f arg k =
    match f with
    | FnClosure (fname, formal, payload) ->
      let env = Env.empty () in
      Env.def env fname f;
      Env.def env formal arg;
      eval (Dispatch.fnbody_force payload) env k

  and apply_primop op vals conts env k =
    match op with
    | Expr (_, f) -> continue (f vals) k
    (* FIXME: should continue with empty tuple: *)
    | Stmt (_, f) -> f vals; continue (Bool false) k
    | Ctrl (_, f) -> eval (f vals conts) env k in
  eval ast (Bootstrap.env ()) Halt

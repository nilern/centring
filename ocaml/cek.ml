open Data
open Core.Std
module Env = Environment

(* FIXME: exhaustive matches *)

type cont = Fn of ast * env * cont
          | Arg of value * env * cont
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
      let split_close (clauses, body) =
        let len = Array.length clauses in
        let cond_step i =
          if i >= len
          then None
          else Some ((clauses.(i), body, env), i + 1) in
        Sequence.unfold 0 cond_step in
      let len = Array.length methods in
      let meth_step i =
        if i >= len
        then None
        else Some (methods.(i), i + 1) in
      let payload = 
        Pending (Sequence.bind (Sequence.unfold 0 meth_step) split_close) in
      continue (FnClosure (name, formal, ref payload)) k
    | Data.App (f, args) ->
      eval f env (Fn (args, env, k))
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
    | Id name ->
      continue (Env.lookup env name) k
    | Const v ->
      continue v k

  and continue v k =
    match k with
    | Fn (arg, env, k') ->
      eval arg env (Arg (v, env, k'))
    | Arg (f, env, k') ->
      apply f v k'
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
    | FnClosure (_, formal, {contents = Done (body, _)}) ->
      eval body (Env.extend Env.empty formal arg) k

  and apply_primop op vals conts env k =
    match op with
    | Expr (_, f) -> continue (f vals) k
    (* FIXME: should continue with empty tuple: *)
    | Stmt (_, f) -> f vals; continue (Bool false) k
    | Ctrl (_, f) -> eval (f vals conts) env k in
  eval ast Env.empty Halt

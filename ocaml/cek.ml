open Data
open Core.Std
module Env = Environment

type cont = Primop of primop * value list * ast array * int * ast array
                    * env * cont
          | Do of ast array * int * env * cont
          | Halt

let rec run ctrl env k =
  match ctrl with
  | Data.Primop (op, [||], conts) ->
    apply op [||] conts env k
  | Data.Primop (op, args, conts) ->
    run args.(0) env (Primop (op, [], args, 0, conts, env, k))
  | Data.Do [||] ->
    (* FIXME: should continue with empty tuple: *)
    continue (Bool false) k
  | Data.Do stmts ->
    run stmts.(0) env (Do (stmts, 0, env, k))
  | Const v ->
    continue v k

and continue v k =
  match k with
  | Primop (op, vals, args, i, conts, env, k') ->
    let i' = i + 1 in
    let vals' = v::vals in
    if i' = Array.length args
    then apply op (Array.of_list_rev vals') conts env k'
    else run args.(i') env (Primop (op, vals', args, i', conts, env, k'))
  | Do (stmts, i, env, k') ->
    let i' = i + 1 in
    if i' = Array.length stmts
    then continue v k'
    else run stmts.(i') env (Do (stmts, i', env, k'))
  | Halt -> v

and apply op vals conts env k =
  match op with
  | Expr f -> continue (f vals) k
  (* FIXME: should continue with empty tuple: *)
  | Stmt f -> f vals; continue (Bool false) k
  | Ctrl f -> run (f vals conts) env k

let interpret ast = run ast Env.empty Halt

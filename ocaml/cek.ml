open Data
open Core.Std
module Env = Environment

type cont = Primop of primop * value list * ast array * int * (ast array) option
                    * env * cont
          | Halt

let rec run ctrl env k =
  match ctrl with
  | Data.Primop (op, args, conts) ->
    run args.(0) env (Primop (op, [], args, 0, conts, env, k))
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
  | Halt -> v

and apply op vals conts env k =
  match op with
  | Expr f -> continue (f vals) k
  (* FIXME: should continue with empty tuple: *)
  | Stmt f -> f vals; continue (Bool false) k
  | Ctrl f -> run (f vals (Option.value_exn conts)) env k

let interpret ast = run ast Env.empty Halt

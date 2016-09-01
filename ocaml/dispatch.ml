open Core.Std
open Data

let bior = Primops.get "bior" |> Option.value_exn
let band = Primops.get "band" |> Option.value_exn
let bnot = Primops.get "bnot" |> Option.value_exn

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

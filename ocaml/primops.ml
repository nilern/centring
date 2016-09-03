open Core.Std
open Data

(* FIXME: exhaustive pattern matches *)

let primops = Hashtbl.create ~hashable:String.hashable ()

let let_expr name f = Hashtbl.set primops ~key:name ~data:(Expr (name, f))
let let_stmt name f = Hashtbl.set primops ~key:name ~data:(Stmt (name, f))
let let_ctrl name f = Hashtbl.set primops ~key:name ~data:(Ctrl (name, f))

let get name = Hashtbl.find primops name

(* Ops *)

let fn_merge = function
  | [|FnClosure (name, formal1, {contents = Done (_, meths1) | Pending meths1});
      FnClosure (_, formal2, {contents = Done (_, meths2) | Pending meths2})|] ->
    let replace_formal = function
      | Id name when name = formal2 -> Id formal1
      | node -> node in
    let replace_atom = function
      | Base ast -> Base (postwalk replace_formal ast)
      | Not ast -> Not (postwalk replace_formal ast) in
    let replace_meth_formal = function
      | (clause, body, env) ->
        (Array.map replace_atom clause, postwalk replace_formal body, env) in
    let meths2' = Sequence.map meths2 replace_meth_formal in
    FnClosure (Symbol.gensym name, formal1,
               ref (Pending (Sequence.append meths1 meths2')))

let () =
  let_expr "fn-merge" fn_merge;
  let_expr "rlen" (fun [|Record (_, fs)|] -> Int (Array.length fs));
  let_expr "rref" (fun [|Record (_, fs); Int i|] -> fs.(i));
  let_stmt "rset" (fun [|Record (_, fs); Int i; v|] -> fs.(i) <- v);
  let_expr "iadd" (fun [|Int a; Int b|] -> Int (a + b));
  let_expr "isub" (fun [|Int a; Int b|] -> Int (a - b));
  let_expr "imul" (fun [|Int a; Int b|] -> Int (a * b));
  let_expr "idiv" (fun [|Int a; Int b|] -> Int (a / b));
  let_expr "bior" (fun [|Bool a; Bool b|] -> Bool (a || b));
  let_expr "band" (fun [|Bool a; Bool b|] -> Bool (a && b));
  let_expr "bnot" (fun [|Bool a|] -> Bool (not a))

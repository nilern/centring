open Core.Std
open Data

(* FIXME: exhaustive pattern matches *)

let primops = Hashtbl.create ~hashable:String.hashable ()

let let_expr name f = Hashtbl.set primops ~key:name ~data:(Expr (name, f))
let let_stmt name f = Hashtbl.set primops ~key:name ~data:(Stmt (name, f))
let let_ctrl name f = Hashtbl.set primops ~key:name ~data:(Ctrl (name, f))

let get name = Hashtbl.find primops name

(* Ops *)

let () = 
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

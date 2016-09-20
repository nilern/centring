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
      | Var (name, pos) when name = formal2 -> Var (formal1, pos)
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

  let_expr "eq?" (fun [|a; b|] -> Bool (a == b));
  let_expr "type" (function
    | [|Int _|] -> Bootstrap.int_t
    | [|Bool _|] -> Bootstrap.bool_t
    | [|Char _|] -> Bootstrap.char_t
    | [|Symbol _|] -> Bootstrap.symbol_t
    | [|List []|] -> Bootstrap.nil_t
    | [|List _|] -> Bootstrap.pair_t
    | [|Stx _ |]-> Bootstrap.stx_t
    | [|Id _|] -> assert false
    | [|FnClosure _|] -> Bootstrap.fn_t
    | [|Record (t, _)|] | [|Bytes (t, _)|] -> t);

  let_expr "rec"  (fun args -> Record (args.(0), Array.subo args ~pos:1));
  let_expr "rlen" (fun [|Record (_, fs)|] -> Int (Array.length fs));
  let_expr "rref" (fun [|Record (_, fs); Int i|] -> fs.(i));
  let_stmt "rset" (fun [|Record (_, fs); Int i; v|] -> fs.(i) <- v);

  let_expr "car" (fun [|List (v::_)|] -> v);
  let_expr "cdr" (fun [|List (_::v)|] -> List v);
  let_expr "cons" (fun [|x; List xs|] -> List (x::xs));

  let_expr "iadd" (fun [|Int a; Int b|] -> Int (a + b));
  let_expr "isub" (fun [|Int a; Int b|] -> Int (a - b));
  let_expr "imul" (fun [|Int a; Int b|] -> Int (a * b));
  let_expr "idiv" (fun [|Int a; Int b|] -> Int (a / b));

  let_expr "ieq?" (fun [|Int a; Int b|] -> let open Int in Bool (a = b));

  let_expr "bior" (fun [|Bool a; Bool b|] -> Bool (a || b));
  let_expr "band" (fun [|Bool a; Bool b|] -> Bool (a && b));
  let_expr "bnot" (fun [|Bool a|] -> Bool (not a));

  let_expr "stx" (fun [|expr; Stx (_, ctx, pos)|] -> Stx (expr, ctx, pos));
  let_expr "stx-expr" (fun [|Stx (e, _, _)|] -> e);

  let_ctrl "brf" (fun [|(Bool a)|] [|thn; els|] -> if a then thn else els);

  let_stmt "err" (fun [|kind; Stx (msg, _, pos)|] ->
                   raise (Ctr_error (kind, msg, pos)))

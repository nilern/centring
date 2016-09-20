open Core.Std
module Env = Environment

let sexp_of_bytes _ = Sexp.Atom "<bytes>"

type src_info = {filename: string; index: int; row: int; col: int}
                [@@deriving sexp_of]

module Phase = Int

type ctx = (Scope.Set.t) Phase.Map.t

type env = (Symbol.t, value) Env.t

and ast = Fn of Symbol.t * Symbol.t * (condition * ast) array * src_info
        | App of ast * ast * src_info
        | Def of Symbol.t * ast * src_info
        | Primop of primop * ast array * ast array * src_info
        | Closure of env * ast * src_info
        | Do of ast array * src_info
        | Var of Symbol.t * src_info
        | Const of value * src_info

and value = Int of int
          | Bool of bool
          | Char of char
          | Symbol of Symbol.t
          | List of value list
          | Stx of value * ctx * src_info
          | Id of value
          | FnClosure of Symbol.t * Symbol.t * fnbody ref
          | Record of value * value array
          | Bytes of value * bytes

and primop = Expr of string * (value array -> value)
           | Stmt of string * (value array -> unit)
           | Ctrl of string * (value array -> ast array -> ast)

and fnbody = Done of ast * (clause * ast * env) Sequence.t
           | Pending of (clause * ast * env) Sequence.t

and atom = Not of ast
         | Base of ast

and clause = atom array

and condition = clause array

(* Accessors *)

let atom_ast = function
  | Base e -> e
  | Not e -> e

let ast_pos = function
  | Fn (_, _, _, pos) -> pos
  | App (_, _, pos) -> pos
  | Def (_, _, pos) -> pos
  | Primop (_, _, _, pos) -> pos
  | Closure (_, _, pos) -> pos
  | Do (_, pos) -> pos
  | Var (_, pos) -> pos
  | Const (_, pos) -> pos

(* Comparisons *)

let rec value_equal v1 v2 =
  match (v1, v2) with
  | (Int i1, Int i2) ->
    let open Int in i1 = i2
  | (Bool b1, Bool b2) ->
    let open Bool in b1 = b2
  | (Char c1, Char c2) ->
    let open Char in c1 = c2
  | (Symbol s1, Symbol s2) ->
    let open Symbol in s1 = s2
  | (List l1, List l2) ->
    List.equal l1 l2 value_equal
  | (Stx (v1, scopes1, pos1), Stx (v2, scopes2, pos2)) ->
    value_equal v1 v2
    && Map.equal Scope.Set.equal scopes1 scopes2
    && pos1 == pos2
  | (Id v1, Id v2) ->
    value_equal v1 v2
  | (FnClosure (name1, formal1, body1), FnClosure (name2, formal2, body2)) ->
    v1 == v2
  | (Record (t1, vs1), Record (t2, vs2)) ->
    t1 == t2
    && Array.equal vs1 vs2 value_equal
  | (Bytes (t1, bs1), Bytes (t2, bs2)) ->
    t1 == t2
    && bs1 = bs2
  | _ -> false

let rec ast_equal ast1 ast2 =
  let atom_equal a1 a2 =
    match (a1, a2) with
    | (Base e1, Base e2) | (Not e1, Not e2) ->
      ast_equal e1 e2
    | _ -> false in
  let cond_equal = Array.equal  ~equal:(Array.equal ~equal:atom_equal) in
  let case_equal (cond1, body1) (cond2, body2) =
    cond_equal cond1 cond2
    && ast_equal body1 body2 in
  match (ast1, ast2) with
  | (Fn (name1, formal1, cases1, _), Fn (name2, formal2, cases2, _)) ->
    let open Symbol in
    name1 = name2
    && formal1 = formal2
    && Array.equal cases1 cases2 case_equal
  | (App (callee1, arg1, _), App (callee2, arg2, _)) ->
    ast_equal callee1 callee2 && ast_equal arg1 arg2
  | (Def (name1, expr1, _), Def (name2, expr2, _)) ->
    let open Symbol in name1 = name2 && ast_equal expr1 expr2
  | (Primop (op1, args1, conts1, _), Primop (op2, args2, conts2, _)) ->
    op1 == op2
    && Array.equal args1 args2 ast_equal
    && Array.equal conts1 conts2 ast_equal
  | (Closure (env1, expr1, _), Closure (env2, expr2, _)) ->
    env1 == env2 && ast_equal expr1 expr2
  | (Do (stmts1, _), Do (stmts2, _)) ->
    Array.equal stmts1 stmts2 ast_equal
  | (Var (name1, _), Var (name2, _)) ->
    let open Symbol in name1 = name2
  | (Const (v1, _), Const (v2, _)) ->
    value_equal v1 v2
  | _ -> false

(* Syntax Object Scope Operations *)

let get_scopes phase (Stx (_, ctx, _)) =
  Option.value (Map.find ctx phase) ~default:Scope.Set.empty

let rec scope_adder f phase scope stx =
  match stx with
  | Stx (List stxen, ctx, pos) ->
    Stx (List (List.map stxen (scope_adder f phase scope)),
         Map.update ctx phase (f scope), pos)
  | Stx (value, ctx, pos) ->
    Stx (value, Map.update ctx phase (f scope), pos)

let add_scope =
  let insert_scope scope = function
    | Some scopes -> Set.add scopes scope
    | None -> Scope.Set.singleton scope in
  scope_adder insert_scope

let flip_scope =
  let flip_scope scope = function
    | Some scopes ->
      if Set.mem scopes scope
      then Set.remove scopes scope
      else Set.add scopes scope
    | None -> Scope.Set.singleton scope in
  scope_adder flip_scope

(* Traversals *)

let atom_map f = function
  | Base ast -> Base (f ast)
  | Not ast -> Not (f ast)

let rec ast_map f = function
  | Fn (name, formal, methods, pos) ->
    let meth_map (clauses, body) =
      (Array.map ~f:(Array.map ~f:(atom_map (ast_map f))) clauses,
       ast_map f body) in
    Fn (name, formal, Array.map meth_map methods, pos)
  | App (callee, arg, pos) ->
    App (f callee, f arg, pos)
  | Primop (op, args, conts, pos) ->
    Primop (op, Array.map f args, Array.map f conts, pos)
  | Closure (env, ast', pos) ->
    Closure (env, f ast', pos)
  | Do (stmts, pos) ->
    Do (Array.map f stmts, pos)
  | (Var _ as node) | (Const _ as node) ->
    node

let walk inner outer ast = outer (ast_map inner ast)

let rec postwalk f ast = walk (postwalk f) f ast

(* Conversions *)

let rec sexp_of_stx = function
  | Stx (payload, ctx, _) ->
    let sexp_of_payload = function
      | List stxen -> Sexp.List (List.map stxen sexp_of_stx)
      | v -> sexp_of_value v in
    let sexp_of_ctx ctx =
      Map.to_alist ctx
      |> List.map ~f:(fun (k, v) ->
                        Sexp.List [Phase.sexp_of_t k; Scope.Set.sexp_of_t v])
      |> Sexp.List in
    Sexp.List [Sexp.Atom "Stx"; sexp_of_payload payload; sexp_of_ctx ctx]

and sexp_of_value = function
  | Int i -> Int.sexp_of_t i
  | Bool b -> if b then Sexp.Atom "#t" else Sexp.Atom "#f"
  | Char c -> Sexp.Atom (String.of_char_list ['#'; '\\'; c])
  | Symbol s -> Symbol.sexp_of_t s
  | List es -> Sexp.List (List.map es sexp_of_value)
  | Stx (e, _, _) -> Sexp.List [Sexp.Atom "Stx"; sexp_of_value e]
  | Id v -> Sexp.List [Sexp.Atom "Id"; sexp_of_value v]
  | FnClosure (name, _ , _) ->
    Sexp.Atom (sprintf "#<Fn %s>" (Symbol.to_string name))
  | Record (_, _) -> Sexp.Atom "#<record>"
  | Bytes (_, _) -> Sexp.Atom "#<bytes>"

let rec sexp_of_atom = function
  | Not e -> Sexp.List [Sexp.Atom "Not"; sexp_of_ast e]
  | Base e -> sexp_of_ast e

and sexp_of_ast = function
  | Fn (name, formal, cases, _) ->
    let sexp_of_case (cond, body) =
      Sexp.List [Array.sexp_of_t (Array.sexp_of_t sexp_of_atom) cond;
                 sexp_of_ast body] in
    Sexp.List (Sexp.Atom "$fn"
               ::(Sexp.Atom (Symbol.to_string name))
               ::(Sexp.Atom (Symbol.to_string formal))
               ::Array.(map sexp_of_case cases |> to_list))
  | App (callee, arg, _) ->
    Sexp.List [Sexp.Atom "$apply"; sexp_of_ast callee; sexp_of_ast arg]
  | Def (name, expr, _) ->
    Sexp.List [Sexp.Atom "$def";
               Sexp.Atom (Symbol.to_string name);
               sexp_of_ast expr]
  | Primop ((Expr (opname, _) | Stmt (opname, _) | Ctrl (opname, _)),
            args, [||], _) ->
    Sexp.List (Sexp.Atom ("%" ^ opname)
               ::Array.(map sexp_of_ast args |> to_list))
  | Primop ((Expr (opname, _) | Stmt (opname, _) | Ctrl (opname, _)),
            args, conts, _) ->
    Sexp.List (Sexp.Atom ("%" ^ opname)
               ::(Array.(map sexp_of_ast args |> to_list))
                  @ (Array.(map sexp_of_ast conts |> to_list)))
  | Closure (_, expr, _) ->
    Sexp.List [Sexp.Atom "Closure"; sexp_of_ast expr]
  | Do (stmts, _) ->
    Sexp.List (Sexp.Atom "$do"::Array.(map sexp_of_ast stmts |> to_list))
  | Var (name, _) ->
    Symbol.sexp_of_t name
  | Const (Symbol sym, _) ->
    Sexp.List [Sexp.Atom "$quote"; Symbol.sexp_of_t sym]
  | Const (v, _) ->
    sexp_of_value v

(* Exceptions *)

exception Ctr_error of value * value * src_info [@@deriving sexp_of]

exception Primop_not_found of string * src_info [@@deriving sexp_of]
exception Unrecognized_sf of string * src_info [@@deriving sexp_of]
exception Sf_args of string * value list * src_info [@@deriving sexp_of]
exception Invalid_case of value * src_info [@@deriving sexp_of]

exception Not_in_scope of Symbol.t * Scope.Set.t * src_info [@@deriving sexp_of]
exception Unbound of Symbol.t * src_info [@@deriving sexp_of]

exception Type_error of value * value * src_info [@@deriving sexp_of]
exception Uncallable of value * src_info [@@deriving sexp_of]

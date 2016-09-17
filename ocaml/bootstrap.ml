open Data

let rec type_t =
  Record (type_t, [|Bool true;
  	                Symbol (Symbol.of_string "Type");
  	                Symbol (Symbol.of_string "stretchy?");
  	                Symbol (Symbol.of_string "name");
  	                Symbol (Symbol.of_string "field-names")|])

let builtin_t name =
  Record (type_t, [|Bool false; Symbol (Symbol.of_string name)|])

let int_t = builtin_t "Int"
let bool_t = builtin_t "Bool"
let char_t = builtin_t "Char"
let symbol_t = builtin_t "Symbol"
let nil_t = builtin_t "List.Empty"
let pair_t = builtin_t "List.Pair"
let stx_t = builtin_t "Syntax"
let fn_t = builtin_t "Fn"

let tuple_t = Record (type_t, [|Bool true;
                                Symbol (Symbol.of_string "Tuple");
                                Symbol (Symbol.of_string "vals")|])

let build_in env name v =
  let sym = Symbol.of_string name in
  let sym' = Symbol.gensym sym in
  Id_store.add_binding sym Scope.Set.empty sym';
  Env.def env sym' v

let env () =
  let res = Env.empty () in
  build_in res "Type" type_t;
  build_in res "Int" int_t;
  build_in res "Bool" bool_t;
  build_in res "Char" char_t;
  build_in res "Symbol" type_t;
  build_in res "List.Empty" nil_t;
  build_in res "List.Pair" pair_t;
  build_in res "Syntax" stx_t;
  build_in res "Fn" fn_t;

  build_in res "Tuple" tuple_t;
  res

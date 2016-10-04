open Core.Std
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
let macro_t = Record (type_t, [|Bool false;
                                Symbol (Symbol.of_string "Macro");
                                Symbol (Symbol.of_string "callable")|])

let build_in (env_ct, env_rt) name v =
  let sym = Symbol.of_string name in
  let sym_ct = Symbol.gensym sym in
  let sym_rt = Symbol.gensym sym in
  Id_store.add_binding sym (Scope.Set.singleton (Scope.Root 1)) sym_ct;
  Env.def env_ct sym_ct v;
  Env.def env_ct sym_rt (Id (Stx (Symbol sym, Phase.Map.empty,
                                  {filename = ""; index = 0; row = 1; col = 0})));
  Id_store.add_binding sym (Scope.Set.singleton (Scope.Root 0)) sym_rt;
  Env.def env_rt sym_rt v

let mmis_sym = Symbol.of_string "max-method-indices"
let mmis_sym_rt = Symbol.gensym mmis_sym
let mmis_stx = Stx (Symbol mmis_sym, Phase.Map.empty,
                    {filename = ""; index = 0; row = 1; col = 0})
let mmis_id = Id mmis_stx

let envs () =
  let (env_ct, _) as envs = (Env.empty (), Env.empty ()) in
  build_in envs "Type" type_t;
  build_in envs "Int" int_t;
  build_in envs "Bool" bool_t;
  build_in envs "Char" char_t;
  build_in envs "Symbol" symbol_t;
  build_in envs "List.Empty" nil_t;
  build_in envs "List.Pair" pair_t;
  build_in envs "Syntax" stx_t;
  build_in envs "Fn" fn_t;

  build_in envs "Tuple" tuple_t;
  build_in envs "Macro" macro_t;

  Env.def env_ct mmis_sym_rt mmis_id;
  Id_store.add_binding mmis_sym (Scope.Set.singleton (Scope.Root 0)) mmis_sym_rt;

  envs

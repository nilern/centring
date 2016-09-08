open Data

let rec type_t =
  Record (type_t, [|Bool true;
  	                Symbol (Symbol.of_string "Type");
  	                Symbol (Symbol.of_string "stretchy?");
  	                Symbol (Symbol.of_string "name");
  	                Symbol (Symbol.of_string "field-names")|])

let env () =
  let res = Env.empty () in
  Env.def res (Symbol.of_string "Type") type_t;
  res

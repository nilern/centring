open Core.Std

module Parser = struct
  module String_seq = struct
    type t = {string: string; index: int}

    let of_string s = {string = s; index = 0}

    let hd {string = s; index = i} =
      if i >= String.length s
      then None
      else Some (s.[i])

    let tl ({string = s; index = i} as ssq) =
      if i >= String.length s
      then ssq
      else {ssq with index = i + 1}
  end

  type 'a t = Parser of (String_seq.t -> (('a, string) Result.t * String_seq.t))
  type 'a _t = 'a t

  let parse (Parser pf) ssq = pf ssq

  (* Monad interface *)
  module M: Monad.Basic with type 'a t = 'a t = struct
    type 'a t = 'a _t

    let bind p f =
      Parser (fun ssq ->
        match parse p ssq with
        | (Ok v, ssq') -> parse (f v) ssq'
        | (Error _, _) as err -> err)

    let return v = Parser (fun ssq -> (Ok v, ssq))

    let map = `Define_using_bind
  end
  include Monad.Make(M)

  (* MonadPlus *)
  (* TODO: formalize MonadPlus notion *)
  let (<|>) p q = Parser (fun ssq ->
    match parse p ssq with
    | (Ok _, _) as res -> res
    | (Error _, _) -> parse q ssq)

  let zero = Parser (fun ssq -> (Error "mzero", ssq))

  (* Primitive parsers *)
  let fail msg = Parser (fun ssq -> (Error msg, ssq))

  let any_char = Parser (fun ssq ->
    match String_seq.hd ssq with
    | Some c -> (Ok c, String_seq.tl ssq)
    | None -> (Error "EOF reached.", ssq))

  let sat p = any_char >>= (fun c ->
    if p c
    then return c
    else fail "predicate unsatisfied")

  let char c = sat (fun d -> c = d)

  let none_of s = any_char >>= (fun c ->
    if not (String.mem s c)
    then return c
    else fail (sprintf "didn't want none of %s" s))

  (* More combinators *)
  let maybe p = map p (fun v -> Some v) <|> return None

  let rec many p = many_one p <|> return []

  and many_one p = p >>= (fun a ->
    many p >>= (fun aa ->
    return (a::aa)))

  let between s e p = s >>= (fun _ ->
    p >>= (fun a ->
    e >>= (fun _ ->
    return a)))

  let surr sr p = between sr sr p

  let until e p = many p >>= (fun vs ->
    e >>= (fun _ ->
    return vs))

  let tabular table = any_char >>= (fun c ->
    match Hashtbl.find table c with
    | Some p -> p
    | None -> fail (sprintf "no readtable entry for %c" c))
end

let ws_char = Parser.sat Char.is_whitespace
let comment = 
  let open Parser in
  char ';' >>= (fun c ->
  many (none_of "\n\r") >>= (fun cs ->
  return (String.of_char_list (c::cs))))

let ws = Parser.many_one ws_char

let digit = Parser.sat Char.is_digit
let int =
  let int_of_char_list cs = cs
    |> String.of_char_list 
    |> Int.of_string
    |> (fun i -> Data.Atom (Int i)) in
  Parser.map (Parser.many_one digit) int_of_char_list

let symchar = Parser.none_of " \t\r\n;#()[]{}"
let symbol =
  let sym_of_char_list cs = cs
    |> String.of_char_list 
    |> Symbol.of_string
    |> (fun sym -> Data.Atom (Symbol sym)) in
  Parser.map (Parser.many_one symchar) sym_of_char_list

let readtable = Hashtbl.create ~hashable: Char.hashable ()
let sharptable = Hashtbl.create ~hashable: Char.hashable ()

let expr =
  let open Parser in
  surr (maybe ws)
       (tabular readtable 
        <|> int 
        <|> symbol)

let read_string s =
  let open Parser in
  let (res, _) = parse expr (String_seq.of_string s) in
  res

(* Init *)

let () =
  let open Parser in
  let open Data in
  let quote = Atom (Symbol (Symbol.of_string "quote")) in
  let newtup = [Atom (Symbol (Symbol.of_string "new"));
                Atom (Symbol (Symbol.of_string "ctr.lang/Tuple"))] in
  Hashtbl.set readtable ~key:'(' 
                        ~data:(map (until (char ')') expr)
                                   (fun es -> List es));
  Hashtbl.set readtable ~key:'\'' ~data:(map expr 
                                             (fun e -> List [quote; e]));
  Hashtbl.set readtable ~key:'#' ~data:(tabular sharptable);
  Hashtbl.set sharptable ~key:'('
                         ~data:(map (until (char ')') expr) 
                                    (fun es -> List (newtup @ es)));
  Hashtbl.set sharptable ~key:'t' ~data:(return (Atom (Bool true)));
  Hashtbl.set sharptable ~key:'f' ~data:(return (Atom (Bool false)))
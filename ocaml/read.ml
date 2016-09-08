open Core.Std
open Data

(* FIXME: src_info.filename *)
(* MAYBE: support just \r as endline *)

module Parser = struct
  module String_seq = struct
    type t = {string: string; pos: src_info}

    let of_string s = {
      string = s;
      pos = {filename = ""; index = 0; row = 1; col = 1}
    }

    let hd {string = s; pos = {index = i; _}} =
      if i >= String.length s
      then None
      else Some (s.[i])

    let tl ({string = s; pos = {index; row; col; _} as pos} as ssq) =
      if index >= String.length s
      then ssq
      else if s.[index] = '\n'
           then {ssq with pos = {pos with index = index + 1;
                                          row = row + 1;
                                          col = col + 1}}
           else {ssq with pos = {pos with index = index + 1;
                                          col = col + 1}}
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

  let (>>) p q = p >>= (fun _ -> q)

  let get_pos = Parser (fun ({pos; _} as ssq) -> (Ok pos, ssq))

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

  let peek = Parser (fun ssq ->
    match String_seq.hd ssq with
    | Some c -> (Ok c, ssq)
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

  let between s e p = s >> p >>= (fun a -> e >> return a)

  let surr sr p = between sr sr p

  let tabular table = peek >>= (fun c ->
    match Hashtbl.find table c with
    | Some p -> p
    | None -> fail (sprintf "no readtable entry for %c" c))
end

open Parser

let ws_char = sat Char.is_whitespace
let comment =
  char ';' >>= (fun c ->
  many (none_of "\n\r") >>= (fun cs ->
  return (c::cs)))

let ws = many_one (many_one ws_char <|> comment)

let digit = sat Char.is_digit
let int =
  let int_of_char_list pos cs = cs
    |> String.of_char_list 
    |> Int.of_string
    |> (fun i -> Stx (Int i, Phase.Map.empty, pos)) in
  get_pos >>= (fun pos ->
    map (many_one digit) (int_of_char_list pos))

let symchar = none_of " \t\r\n;#()[]{}"
let isymchar = none_of " \t\r\n;()[]{}"
let symstr = (many_one symchar)
let isymstr = (many_one isymchar)
let sym_of_char_list pos cs = cs
  |> String.of_char_list
  |> Symbol.of_string
  |> (fun sym -> Stx (Symbol sym, Phase.Map.empty, pos))
let isym_of_char_list pos cs =
  Stx (Symbol (Symbol.of_string ("##" ^ String.of_char_list cs)),
       Phase.Map.empty, pos)
let symbol = get_pos >>= (fun pos -> map symstr (sym_of_char_list pos))
let isymbol = get_pos >>= (fun pos -> 
  char '#' >> map isymstr (isym_of_char_list pos))

let bool c b = get_pos >>= (fun pos ->
  char c >> return (Stx (Bool b, Phase.Map.empty, pos)))

let readtable = Hashtbl.create ~hashable: Char.hashable ()
let sharptable = Hashtbl.create ~hashable: Char.hashable ()

let expr =
  surr (maybe ws)
       (tabular readtable 
        <|> int 
        <|> symbol)

let list = get_pos >>= (fun pos ->
  map (between (char '(') (char ')') (many expr))
      (fun es -> Stx (List es, Phase.Map.empty, pos)))

let quote = get_pos >>= (fun pos ->
  char '\'' >> expr >>= (fun e ->
    return (Stx (List [Stx (Symbol (Symbol.of_string "quote"), Phase.Map.empty, pos);
                       e], Phase.Map.empty, pos))))

let tuple = get_pos >>= (fun pos ->
  let nw = Stx (Symbol (Symbol.of_string "new"), Phase.Map.empty, pos) in
  let tup = Stx (Symbol (Symbol.of_string "Tuple"), Phase.Map.empty, pos) in
  map (between (char '(') (char ')') (many expr))
      (fun es -> Stx (List (nw::tup::es), Phase.Map.empty, pos)))

let read_string s =
  let (res, _) = parse expr (String_seq.of_string s) in
  res

let read_all s =
  let (res, _) = parse (many expr) (String_seq.of_string s) in
  Result.map res
             (fun (Stx (_, s, pos)::_ as es) ->
               Stx (List ((Stx (Symbol (Symbol.of_string "##sf#do"), s, pos))
                          ::es), s, pos))

(* Init *)

let () =
  let set = Hashtbl.set in
  set readtable ~key:'(' ~data:list;
  set readtable ~key:'\'' ~data:quote;
  set readtable ~key:'#' ~data:(any_char >> tabular sharptable);

  set sharptable ~key:'(' ~data:tuple;
  set sharptable ~key:'t' ~data:(bool 't' true);
  set sharptable ~key:'f' ~data:(bool 'f' false);
  set sharptable ~key:'#' ~data:isymbol

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

open Parser

let ws_char = sat Char.is_whitespace
let comment =
  char ';' >>= (fun c ->
  many (none_of "\n\r") >>= (fun cs ->
  return (String.of_char_list (c::cs))))

let ws = many_one ws_char

let digit = sat Char.is_digit
let int =
  let int_of_char_list pos cs = cs
    |> String.of_char_list 
    |> Int.of_string
    |> (fun i -> {
         expr = Data.Atom (Int i); 
         scopes = String.Set.empty;
         src = pos
  }) in
  get_pos >>= (fun pos ->
    map (many_one digit) (int_of_char_list pos))

let symchar = none_of " \t\r\n;#()[]{}"
let isymchar = none_of " \t\r\n;()[]{}"
let symstr = (many_one symchar)
let isymstr = (many_one isymchar)
let sym_of_char_list pos cs = cs
  |> String.of_char_list
  |> Symbol.of_string
  |> (fun sym -> {
         expr = Data.Atom (Symbol sym);
         scopes = String.Set.empty;
         src = pos
  })
let isym_of_char_list pos cs = {
  expr = Data.Atom (Symbol (Symbol.of_string ("##" ^ String.of_char_list cs)));
  scopes = String.Set.empty;
  src = pos
}
let symbol = get_pos >>= (fun pos -> map symstr (sym_of_char_list pos))
let isymbol = get_pos >>= (fun pos -> map isymstr (isym_of_char_list pos))

let readtable = Hashtbl.create ~hashable: Char.hashable ()
let sharptable = Hashtbl.create ~hashable: Char.hashable ()

let expr =
  surr (maybe ws)
       (tabular readtable 
        <|> int 
        <|> symbol)

let read_string s =
  let (res, _) = parse expr (String_seq.of_string s) in
  res

(* Init *)

let () =
  let stx_list (e::_ as es) = cexp_to_stx e (List es) in
  let make_quote e =
    let quote = cexp_to_stx e (Atom (Symbol (Symbol.of_string "quote"))) in
    cexp_to_stx e (List [quote; e]) in
  let make_tuple (e::_ as es) =
    let nw = cexp_to_stx e (Atom (Symbol (Symbol.of_string "new"))) in
    let tuple =
      cexp_to_stx e (Atom (Symbol (Symbol.of_string "ctr.lang/Tuple"))) in
    cexp_to_stx e (List (nw::tuple::es)) in
  Hashtbl.set readtable ~key:'(' 
                        ~data:(map (until (char ')') expr) stx_list);
  Hashtbl.set readtable ~key:'\'' ~data:(map expr make_quote);
  Hashtbl.set readtable ~key:'#' ~data:(tabular sharptable);
  Hashtbl.set sharptable ~key:'('
                         ~data:(map (until (char ')') expr) make_tuple);
  Hashtbl.set sharptable ~key:'t' 
                         ~data:(get_pos >>= (fun src ->
                                 return {expr = Atom (Bool true);
                                         scopes = String.Set.empty;
                                         src}));
  Hashtbl.set sharptable ~key:'f'  
                         ~data:(get_pos >>= (fun src ->
                                 return {expr = Atom (Bool false);
                                         scopes = String.Set.empty;
                                         src}));
  Hashtbl.set sharptable ~key:'#' ~data:isymbol

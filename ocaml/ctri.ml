open Core.Std

(* Data *)

type symbol = Symbol of string
            | Gensym of int

type value = Int of int
           | Char of char
           | Symbol of symbol
           | Record of value array
           | Bytes of bytes

(* Env *)

module Env = struct
  type t = KV of symbol * value ref * t
         | Empty

  let empty = Empty

  let rec lookup env key =
    match env with
    | KV (k, v, _) when k = key -> !v
    | KV (_, _, env') -> lookup env' key
    | Empty -> assert false

  let extend env k v = KV (k, ref v, env)

  let rec set env key value =
    match env with
    | KV (k, v, _) when k = key -> v := value
    | KV (_, _, env') -> set env' key value
    | Empty -> assert false
end

(* AST *)

type primop = Expr of (value array -> value)
            | Stmt of (value array -> unit)
            | Ctrl of (ast array -> value array -> ast)

and ast = Fn of symbol * symbol * (ast * ast) array
        | Primop of primop * ast array * (ast array) option
        | Closure of Env.t * ast
        | Id of symbol
        | Const of value

(* Read *)

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

module Parser = struct
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

  (* Primitive parsers *)
  let any_char = Parser (fun ssq ->
    match String_seq.hd ssq with
    | Some c -> (Ok c, String_seq.tl ssq)
    | None -> (Error "EOF reached.", ssq))
end

let () =
  let open Parser in
  let read = any_char >>= (fun c ->
    any_char >>= (fun d ->
    return (c, d))) in
  match parse read (String_seq.of_string "foo") with
  | (Ok (_, d), _) -> Out_channel.output_char stdout d
  | _ -> Out_channel.output_string stdout "Nooo!"

open Core.Std

type ('k, 'v) t = KV of 'k * 'v ref * ('k, 'v) t
                | Empty

let empty = Empty

let rec lookup env key =
  match env with
  | KV (k, v, _) when k = key -> !v
  | KV (_, _, env') -> lookup env' key
  | Empty -> assert false

let extend env k v = KV (k, ref v, env)

let rec merge e1 = function
  | KV (k, {contents = v}, e2') -> merge (extend e1 k v) e2'
  | Empty -> e1

let rec set env key value =
  match env with
  | KV (k, v, _) when k = key -> v := value
  | KV (_, _, env') -> set env' key value
  | Empty -> assert false

let sexp_of_t _ _ _ = Sexp.Atom "<env>"

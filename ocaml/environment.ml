open Core.Std

type ('k, 'v) t = ('k, 'v) Hashtbl.t list

let empty () = [Hashtbl.Poly.create ()]

let lookup env key = List.find_map env (fun frame -> Hashtbl.find frame key)

let def env key value = Hashtbl.set (List.hd_exn env) key value

let merge e1 e2 = e2 @ e1

let sexp_of_t _ _ _ = Sexp.Atom "<env>"
open Core.Std
open Core_extended.Std
open Readline
open Data
open Expand
open Analyze
open Cek

let action stx estx ana (env_ct, env_rt) estr =
  let open Result in
  Read.read_all estr >>| (fun cexp ->
    if ana
    then cexp |> expand 0 env_ct |> analyze 0 |> sexp_of_ast
    else if estx
    then cexp |> expand 0 env_ct |> sexp_of_stx
    else if stx
    then cexp |> sexp_of_stx
    else cexp |> expand 0 env_ct |> analyze 0
              |> interpret 0 env_rt |> sexp_of_value)

let act action estr =
  let act_and_print estr =
    try (match action estr with
         | Ok sexp ->
           Sexp.pp_hum Format.std_formatter sexp;
           Format.print_newline ()
         | Error err ->
           Out_channel.output_string stderr err) with
    | e ->
      Printf.eprintf "Error: %s\n" (Exn.to_string e);
      Out_channel.flush stderr in
  match estr with
  | Some estr ->
    act_and_print estr
  | None ->
    let rec loop () =
      (match Readline.input_line ~prompt: "ctr> " () with
       | Some estr ->
         act_and_print estr;
         loop ()
       | None -> ()) in
    loop ()

(* FIXME: need to deal properly with phases and environments *)
(* MAYBE: relative paths *)
let make_load (env_ct, env_rt) =
  PhExpr ("load", (fun phase -> function
    | [|Stx (Symbol filename, _, _)|] ->
      let estr = In_channel.read_all (Symbol.to_string filename) in
      (match Read.read_all estr with
       | Ok cexp ->
         cexp |> expand phase env_ct |> analyze phase
         |> interpret phase (if phase = 0 then env_rt else env_ct)
       | Error msg ->
         assert false))) (* FIXME *)

let command =
  Command.basic
    ~summary:"Centring interpreter"
    Command.Spec.(
      empty
      +> flag "-e" (optional string) ~doc:"expr use expr as input"
      +> flag "--stx" no_arg ~doc:" just parse syntax object and print it"
      +> flag "--estx" no_arg ~doc:" just macroexpand and print result"
      +> flag "--ana" no_arg ~doc:" just build AST and print it"
      +> anon (maybe ("filename" %: file))
    )
    (fun expr_str stx estx ana filename () ->
      let estr = match expr_str with
                 | Some estr -> Some estr
                 | None -> Option.map ~f:In_channel.read_all filename in
      let envs = Bootstrap.envs () in
      Hashtbl.set Primops.primops ~key:"load" ~data:(make_load envs);
      act (action stx estx ana envs) estr)

let () = Command.run command

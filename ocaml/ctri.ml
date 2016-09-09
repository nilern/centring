open Core.Std
open Result
open Data
module Ops = Primops

let action stx estx ana estr =
  Read.read_all estr >>| (fun cexp ->
    (if ana
     then cexp |> Expand.expand (Env.empty ()) |> Analyze.analyze |> Data.sexp_of_ast
     else if estx
     then cexp |> Expand.expand (Env.empty ()) |> Data.sexp_of_value
     else if stx
     then cexp |> Data.sexp_of_value
     else cexp |> Expand.expand (Env.empty ()) |> Analyze.analyze |> Cek.interpret |> Data.sexp_of_value))

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
                | Some estr -> Ok estr
                | None -> 
                  of_option filename "No FILENAME or expr specified"
                  >>| In_channel.read_all in
      match estr >>= action stx estx ana with
      | Ok sexp -> Sexp.pp_hum Format.std_formatter sexp
      | Error msg -> Out_channel.output_string stdout msg)

let () = Command.run command

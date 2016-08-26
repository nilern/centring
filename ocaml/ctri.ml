open Core.Std
open Result
module Ops = Primops

let command =
  Command.basic
    ~summary:"Centring interpreter"
    Command.Spec.(
      empty
      +> flag "-e" (required string) ~doc:"expr use expr as input"
      +> flag "--stx" no_arg ~doc:" just parse syntax object and print it"
      +> flag "--ana" no_arg ~doc:" just build AST and print it"
    )
    (fun expr_str stx ana () ->
      match Read.read_string expr_str with
      | Ok cexp ->
        (if ana
         then cexp |> Analyze.analyze |> Data.sexp_of_ast
         else if stx
         then cexp |> Data.sexp_of_stx
         else cexp |> Analyze.analyze |> Cek.interpret |> Data.sexp_of_value)
        |> Sexp.pp_hum Format.std_formatter
      | Error msg -> Out_channel.output_string stdout msg)

let () = Command.run command

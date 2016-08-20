open Core.Std

let command =
  Command.basic
    ~summary:"Centring interpreter"
    Command.Spec.(
      empty
      +> flag "-e" (required string) ~doc:"expr use expr as input"
    )
    (fun expr_str () ->
      match Read.read_string expr_str with
        | Ok v -> v
          |> Data.sexp_of_cexp
          |> Sexp.pp_hum Format.std_formatter
        | Error msg -> Out_channel.output_string stdout msg)

let () = Command.run command

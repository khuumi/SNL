type action = Raw | Compile

let _ =
  let action = if Array.length Sys.argv > 1
               then List.assoc Sys.argv.(1) [("-r", Raw)]
               else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.tokenize lexbuf in
  match action with
    Raw -> print_string (Ast.program_s program)
  | Compile -> print_string "Not implemented."

type action = Expr | Stmt

let _ =
  let action = if Array.length Sys.argv > 1
               then List.assoc Sys.argv.(1) [("-e", Expr);
                                             ("-s", Stmt)]
               else Stmt in
  let lexbuf = Lexing.from_channel stdin in
  match action with
    Expr -> print_string (Ast.expr_s (Parser.expr Scanner.tokenize lexbuf))
  | Stmt -> print_string (Ast.stmt_s (Parser.stmt Scanner.tokenize lexbuf))

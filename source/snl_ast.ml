type action = Expr | Stmt | Program | Java

let _ =
  let action = if Array.length Sys.argv > 1
               then List.assoc Sys.argv.(1) [("-e", Expr);
                                             ("-s", Stmt);
                                             ("-p", Program);
                                             ("-j", Java)]
               else Program in
  let lexbuf = Lexing.from_channel stdin in
  match action with
    Expr -> print_string (Ast.expr_s (Parser.expr Scanner.tokenize lexbuf))
  | Stmt -> print_string (Ast.stmt_s (Parser.stmt Scanner.tokenize lexbuf))
  | Program -> print_string (Ast.program_s
                               (Parser.program Scanner.tokenize lexbuf))
  | Java -> 
    let str = Codegen.gen_ir "file\n" in
    print_string str
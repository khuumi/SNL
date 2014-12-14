open Analyzer
open Str

type action = Expr | Stmt | Program | Java

(* Usage: ./snl [-e | -s | -p | -j] file [-o output_file] *)

let _ =
  let action = List.assoc Sys.argv.(1) [("-e", Expr);
                                        ("-s", Stmt);
                                        ("-p", Program);
                                        ("-j", Java);] in
  let lexbuf = Lexing.from_channel (open_in Sys.argv.(2)) in
  match action with
    (* expr, stmt, and program are for testing the AST, java is code gen *)
    Expr -> print_string (Ast.expr_s (Parser.expr Scanner.tokenize lexbuf))
  | Stmt -> print_string (Ast.stmt_s (Parser.stmt Scanner.tokenize lexbuf))
  | Program -> print_string (Ast.program_s
                               (Parser.program Scanner.tokenize lexbuf))

  | Java ->
  (* see if file exists and remove if it is already there *)
    let strlst = split (regexp "/") Sys.argv.(2) in
    let path = if Array.length Sys.argv > 3
            then List.assoc Sys.argv.(3) [("-path" ,"./"^Sys.argv.(4)^"/");]
               else "java/" in
     
    let name = String.sub (List.tl strlst) 0 ((String.length (List.tl strlst))-4) in
    let pname = path^name^".java" in

     (* see if file exists and remove if it is already there *)
     let strlst = split (regexp "/") Sys.argv.(2) in 
     let name = (String.sub (List.tl strlst) 0
                   ((String.length Sys.argv.(2)) - 4)) in
     let pname = if Array.length Sys.argv > 3 && Sys.argv.(3) = "--output_path"
                 then Sys.argv.(4) ^ "/" ^ name ^ ".java"
                 else name ^ ".java" in
     if Sys.file_exists pname then Sys.remove(pname);
     let ast = Parser.program Scanner.tokenize lexbuf in
     let sast = Analyzer.annotate_program ast in
     ignore (Codegen.start_gen sast pname);
     print_string (Ast.program_s ast)

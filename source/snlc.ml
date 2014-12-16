(* Usage: ./snl [-e | -s | -p | -j] file [-o output_file] *)

open Analyzer
open Printf
open Sast


type action = Expr | Stmt | Program | Java


let write_out (filename : string) (buffer : string) =
  if Sys.file_exists filename then Sys.remove(filename);
  let file = (open_out_gen
                [Open_creat; Open_wronly; Open_text]
                0o666
                filename) in
  fprintf file "%s" buffer;
  close_out file


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
     let strlst = Str.split (Str.regexp "/") Sys.argv.(2) in
     let tail = List.hd (List.rev strlst) in
     let name = String.sub tail 0 ((String.length tail) - 4)
     and path = if Array.length Sys.argv > 3 && Sys.argv.(3) = "--output_path"
                then Sys.argv.(4) ^ "/"
                else "./" in
     let ast = Parser.program Scanner.tokenize lexbuf in
     let sast = Analyzer.annotate_program ast in
     let diagnostics, any_error = Analyzer.generate_diagnostics sast in
     List.iter print_endline diagnostics;
     if any_error
     then failwith "Errors in program."
     else write_out (path ^ name ^ ".java") (Codegen.gen_main sast.stages name);
     ignore (List.map
               (fun recipe -> write_out
                                (path ^ "Recipe_" ^ recipe.rname ^ ".java")
                                (Codegen.gen_recipe recipe))
               sast.recipes)
     (*print_string (Ast.program_s ast)*)

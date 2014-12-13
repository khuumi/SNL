open Analyzer

type action = Expr | Stmt | Program | Java

(*fake recipe start*)
let add : Ast.op =
        Add

let mynum : Ast.constant =
        Int(1)

let binop_expr : Ast.expr =
        Binop(Constant(mynum), add, Constant(Int(1)))
let return_expr : Ast.expr =
        Return(binop_expr)

let increment : Ast.stmt =
        Expr(return_expr)

let calc : Ast.stage =
        {sname = "calc"; body =[increment] ; is_start = true}
let fake_recipe : Ast.recipe =
        {rname = "inc"; formals = ["num"]; body = [calc];}
(*fake recipe end*)

(*fake stage start*)

let main_stmt1 : Ast.stmt =
        Expr(Assign(Id("i", Global),Constant(Int(1))))

let main_stmt2 : Ast.stmt =
        Expr(Call("inc",[Id("i", Global)]))

let fake_stage : Ast.stage =
        {sname = "first_stage"; body =[main_stmt1; main_stmt2]; is_start = true}
(*fake stage end*)

(*getting bypassed ast*)
let get_ast : Ast.program =
        { recipes = [fake_recipe]; stages=[fake_stage]; }


let _ =
  let action = if Array.length Sys.argv > 1
               then List.assoc Sys.argv.(1) [("-e", Expr);
                                             ("-s", Stmt);
                                             ("-p", Program);
                                             ("-j", Java);]
               else Program in
  let lexbuf = Lexing.from_channel stdin in
  match action with
    Expr -> print_string (Ast.expr_s (Parser.expr Scanner.tokenize lexbuf))
  | Stmt -> print_string (Ast.stmt_s (Parser.stmt Scanner.tokenize lexbuf))
  | Program -> print_string (Ast.program_s
                               (Parser.program Scanner.tokenize lexbuf))
  | Java ->
                  let sast = Analyzer.annotate_program get_ast in
                 (*let sast = (Analyzer.annotate_program
                               (Parser.program Scanner.tokenize lexbuf)) in*)
    ignore (Codegen.start_gen sast Sys.argv.(2))

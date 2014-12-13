open Printf
open Sast

(* let get_header = "{\n  public static void main(String args[])\n"
 *)

let write_out (filename : string) (buffer : string) =
    let file = (open_out_gen [Open_creat; Open_wronly;
    Open_text; Open_append] 0o666
    ("java/" ^ filename ^ ".java")) in

    fprintf file "%s" buffer

let get_initial_stage_header (start_stage_name : string) =
    "{\n public static void main(String args[]){\n" ^start_stage_name ^ "();\n}\n"

let make_header (filename : string) (is_recipe : bool) =
    match is_recipe with
    true -> let header = "public final class " ^ filename ^ "{\n public static
        void perform(){\n" in  write_out filename header
  | false -> let header = "public class " ^ filename ^ "{\n" in  write_out
        filename header

let print_const (const : a_constant) (filename : string) =
    let out = 
    match const with
       AInt(num, _) -> " new SNLObject("^ (string_of_int num) ^"\"int\")"                 
     | AFloat(fl, _) ->" new SNLObject("^ (string_of_float fl) ^"\"float\")"
     | ABool(b, _) -> " new SNLObject("^  (string_of_bool b) ^"\"bool\")"
     | AString(s, _) ->" new SNLObject(" ^ s ^ "\"string\")"
  
  in write_out filename out

let print_expr (expr : a_expr) (filename : string) =
    match expr with
    AConstant(const) -> print_const const filename
  | AId(_, _, t) -> write_out filename "An ID"
  | AUnop(_, _, t) -> write_out filename "A unop"
  | ABinop(_, _, _, t) -> write_out filename "A binop"
  | AAssign(e1) -> write_out filename "An assignment"
  | ANext(_, t) -> write_out filename "a next expression"
  | AReturn(_, t) -> write_out filename "a return statement"
  | AList(_, t) -> write_out filename "a list"
  | AInput(t) -> write_out filename "an input"
  | ACall(_, _, t) -> write_out filename "a call"
  | AAccess(_, _, t) -> write_out filename "n access"


let rec print_stmt (statement : a_stmt) (filename : string) =
    match statement with
        AExpr(e) -> print_expr e filename; write_out filename ";\n"
     | ABlock(e_list) -> List.map (fun expr -> print_expr expr filename;
     write_out filename ";\n") e_list; ()
     | AIf(e, first_stmt, second_stmt) -> write_out filename "if(";
     print_stmt first_stmt filename;
     write_out filename "){";
     print_stmt second_stmt;
     write_out filename "}"

let print_stage (stage : a_stage) (file_name : string) =
    let header = "private static void " ^ stage.sname ^ "(){\n" in
    let initial_header = match stage.is_start with
        true -> get_initial_stage_header stage.sname
      | false -> ""
    in write_out file_name (initial_header ^ header);
    (*  print_string initial_header ^ header; *)
    List.map  (fun body -> print_stmt body file_name) stage.body;
    write_out file_name "}"

let start_gen (sast : a_program) (filename : string) =
    let name =  String.sub filename 0 ((String.length filename) - 4) in
    make_header name false;
    List.map (fun stage -> print_stage stage name) sast.stages


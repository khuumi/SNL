open Printf
open Sast

(* let get_header = "{\n  public static void main(String args[])\n"
*)
let get_initial_stage_header (file_name : string) =
  "{\n public static void main(String args[]){\n" ^file_name ^ "();\n}"

let make_header (file_name : string) (is_recipe : bool) =
  match is_recipe with
    true -> sprintf "public final class %s {\n  public static void perform(){\n" file_name
  | false -> sprintf "public class %s{" file_name

let write_out (filename : string) (buffer : string) =
  let file = (open_out_gen [Open_creat; Open_wronly;
                        Open_text; Open_append] 0o666
                        ("java/" ^ filename ^ ".java")) in

  fprintf file "%s" buffer

let print_const (const : a_constant) (filename : string) =
  write_out filename "const"
(*   match const with
    AInt(num, _) -> write_out filename "[Int: " ^ (string_of_int num) ^ "]"
  | AFloat(fl, _) -> write_out filename "[Float: " ^ (string_of_float fl) ^ "]"
  | ABool(b, _) -> write_out filename "[Bool: " ^ (string_of_bool b) ^ "]"
  | AString(s, _) -> write_out filename "[String: " ^ (string_of_bool num) ^ "]" *)

let print_expr (expr : a_expr) (filename : string) =
  match expr with
  AConstant(const) -> print_const const filename
  | AId(_, _, t) -> write_out filename "An ID"
  | AUnop(_, _, t) -> write_out filename "A unop"
  | ABinop(_, _, _, t) -> write_out filename "A binop"
  | AAssign(expr) -> write_out filename "An assignment"
  | ANext(_, t) -> write_out filename "a next expression"
  | AReturn(_, t) -> write_out filename "a return statement"
  | AList(_, t) -> write_out filename "a list"
  | AInput(t) -> write_out filename "an input"
  | ACall(_, _, t) -> write_out filename "a call"
  | AAccess(_, _, t) -> write_out filename "an access"


let rec print_stmt (statement : a_stmt) (filename : string) =
  match statement with
  AExpr(e) -> print_expr e filename; write_out filename ";"
(*   | ABlock(e_list) -> List.map (print_expr filename) e   Need to add a semi-colon after each expr
*)  | AIf(e, first_stmt, second_stmt) -> write_out filename "if(";
    print_stmt first_stmt filename;
    write_out filename "){";
    print_stmt second_stmt;
    write_out filename "}"

let print_stage (stage : a_stage) (filename : string) (start : bool) =
  let header = "*****" in
  let initial_header = match stage.is_start with
    true -> get_initial_stage_header filename
    | false -> ""

    in write_out filename (initial_header ^ header);
    List.map  (fun body -> print_stmt body filename) stage.body;
    write_out filename "}"

let start_gen (sast : a_program) (filename : string) =
  make_header filename false;
  List.map (fun stage -> print_stage stage filename) sast.stages


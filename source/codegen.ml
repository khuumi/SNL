open Printf
open Sast
open Ast

(* let get_header = "{\n  public static void main(String args[])\n"
 *)


let write_out (filename : string) (buffer : string) =
    let file = (open_out_gen [Open_creat; Open_wronly;
    Open_text; Open_append] 0o666
    (filename)) in

    fprintf file "%s" buffer;
    close_out file
 

let get_initial_stage_header (start_stage_name : string) =
    "\n public static void main(String args[]){\n" ^start_stage_name ^ "();\n}\n"

let make_header (filename : string) (is_recipe : bool) =
    let scanner = "import java.util.Scanner;\n" in 
    let scanner2 = "private static Scanner input = new Scanner(System.in);" in 
    match is_recipe with
    true -> let header = scanner ^ "public final class " ^ filename ^ "{"
    ^ scanner2 ^ "\n public static void perform(){\n" in  write_out filename header
  | false -> let header = scanner ^ "public class " ^ filename ^ "{\n"  ^ scanner2 in write_out
        filename header

let print_const (const : a_constant) (filename : string) =
    let out = 
    match const with
       AInt(num, _) -> " new SNLObject("^ (string_of_int num) ^", \"int\")"                 
     | AFloat(fl, _) ->" new SNLObject("^ (string_of_float fl) ^", \"float\")"
     | ABool(b, _) -> " new SNLObject("^  (string_of_bool b) ^", \"bool\")"
     | AString(s, _) ->" new SNLObject(\"" ^ s ^ "\", \"string\")"
  
  in write_out filename out

let print_id (s : string) (scope : Ast.scope) (file_name : string) =
    write_out file_name "ID"

    
let rec print_expr (expr : a_expr) (filename : string) =
    match expr with
    AConstant(const) -> print_const const filename
  | AId(s, scope, _) -> print_id s scope  filename
  | AUnop(op, e, _) -> print_unop e op filename 
  | ABinop(e1, op, e2 , t) -> print_binop e1 e2 op filename
  | AAssign(e1) -> write_out filename "An assignment"
  | ANext(s, t) -> let next = s ^ "()" in write_out filename next
  | AList(e_list, t) -> write_out filename "list"(*print_list e_list filename*)
  | AReturn(e, t) -> print_return e filename
  | AInput(t) -> write_out filename "new SNLObject(input.nextLine(), \"string\")" 
  | ACall(s, e_list, t) -> print_func_call s e_list filename
  | AAccess(_, _, t) -> write_out filename "n access"

  and print_binop (e1 : a_expr) (e2 : a_expr) (op : Ast.op) (filename : string) = 
    let string_op = 
        match op with 
        Add -> "add"
      | Sub -> "sub"
      | Mult -> "mult"
      | Div -> "div"
      | Equal -> "eq"
      | Neq -> "neq"
      | Gt -> "gt"
      | Geq -> "geq"
      | Lt -> "lt"
      | Leq -> "leq" 
      | And -> "and"
      | Or -> "or" in 
    let output = "." ^ string_op ^ "(" in 
    print_expr e1 filename;
    write_out filename output;
    print_expr e2 filename;
    write_out filename ")"

  and print_unop (e : a_expr) (op : Ast.op) (file_name : string) = 
      let string_op = 
          match op with 
            Negate -> "neg"
          | Not -> "not" in 
      let output = "." ^ string_op ^ "()" in 
      print_expr e file_name;
      write_out file_name output

  and print_func_call (s : string) (e_list : a_expr list) (file_name : string) = 
     match s with 
     "show" -> write_out file_name "System.out.println("; 
                ignore (List.map (fun e -> print_expr e file_name; 
                                        write_out file_name " + "; 
                                        write_out file_name "\"\")") e_list)
            (* INCOMPLETE *)
    | _ -> let call =  "do " ^ s ^ " to ... INCOMPLETE" in(*
            let str_list = List.fold_left 
                (fun a b -> let next = "new SNLObject("")"b::a)*)
            write_out file_name call(*
            ignore (List.map (fun e count -> print_expr e file_name;
                                    write_out*)

            (* INCOMPLETE *)(*
  and print_list (e_list : a_expr list) (file_name : string) =
      let init_list = "new SNLObject(\"list\", " in
      write_out file_name init_list*)
      

and print_return (e : a_expr) (file_name : string) =
    write_out file_name "return ";
    print_expr e file_name

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

let start_gen (sast : a_program) (name : string) =
    make_header name false;
    List.map (fun stage -> print_stage stage name) sast.stages;    
    write_out name "}";
     

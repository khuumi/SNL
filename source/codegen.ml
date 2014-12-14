open Printf
open Sast
open Ast

(* let get_header = "{\n  public static void main(String args[])\n"
 *)

let write_out (filename : string) (buffer : string) =
    let file = (open_out_gen [Open_creat; Open_wronly;
    Open_text; Open_append] 0o666
    ("java/" ^ filename ^ ".java")) in

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

let to_string_const (const : a_constant)  : string=
    match const with
       AInt(num, _) -> " new SNLObject("^ (string_of_int num) ^", \"int\")"                 
     | AFloat(fl, _) ->" new SNLObject("^ (string_of_float fl) ^", \"float\")"
     | ABool(b, _) -> " new SNLObject("^  (string_of_bool b) ^", \"bool\")"
     | AString(s, _) ->" new SNLObject(\"" ^ s ^ "\", \"string\")"


let print_id (s : string) (scope : Ast.scope) (file_name : string) =
    write_out file_name "ID"

let rec to_string_expr (expr : a_expr) : string = 
    match expr with 
      AConstant(const) -> to_string_const const
    | AId(s, scope, _) -> "TODO: id" 
    | AUnop(op, e, _) ->  to_string_unop e op
    | ABinop(e1, op, e2, t) -> to_string_binop e1 e2 op
    | AAssign(e1) -> "TODO: assignment"
    | ANext(s, t) -> s ^ "()"
    | AReturn(e, t) -> "TODO: return"
    | AList(e_list, t) -> "TODO: list" 
    | AInput(t) -> "new SNLObject(input.nextLine(), \"string\")"
    | ACall(s, e_list, t) -> to_string_call s e_list
    | AAccess(i, e, t) -> "TODO: Access"

  and to_string_unop (e : a_expr) (op : Ast.op) : string = 
      let string_op = 
          match op with 
            Negate -> "neg"
          | Not -> "not" in 
      (to_string_expr e) ^ "." ^ string_op ^ "()"

  and to_string_binop (e1 : a_expr) (e2 : a_expr) (op : Ast.op) = 
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
    (to_string_expr e1) ^ "." ^ string_op ^ "(" ^ (to_string_expr e2) ^ ")"

 and to_string_call (name : string) (e_list : a_expr list) : string = 
    match name with 
        "show" -> let list_e_strings = List.rev ( List.fold_left   
                (fun list e -> (to_string_expr e)::list ) [] e_list ) in
                 "System.out.println(" ^ (String.concat " + " list_e_strings) ^")"
      | _ -> let  list_e_strings = List.rev ( List.fold_left 
           (fun list e -> (to_string_expr e)::list ) []  e_list) in 
              name ^ (String.concat ", " list_e_strings) ^ ")" 
               
let rec print_expr (expr : a_expr) (filename : string) =
    match expr with
    AConstant(const) -> print_const const filename
  | AId(s, scope, _) -> print_id s scope  filename
  | AUnop(op, e, _) -> print_unop e op filename 
  | ABinop(e1, op, e2 , t) -> print_binop e1 e2 op filename
  | AAssign(e1) -> write_out filename "An assignment"
  | ANext(s, t) -> let next = s ^ "()" in write_out filename next
  | AReturn(e, t) -> print_return e filename
  | AList(_, t) -> write_out filename "a list"
  | AInput(t) -> write_out filename "new SNLObject(input.nextLine(), \"string\")" 
  | ACall(s, e_list, t) -> write_out filename "call"
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

  (*and print_func_call (s : string) (e_list : a_expr list) (file_name : string) = 
     match s with 
     "show" -> write_out file_name "System.out.println("; 
                ignore (List.map (fun e -> print_expr e file_name; 
                                        write_out file_name " + "; 
                                        write_out file_name "\"\")") e_list) 
            
    | _ -> write_out file_name "do " ^ s ^ " to ... INCOMPLETE" *)
      
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

let rec to_string_stmt (statement : a_stmt) = 
    match statement with 
        AExpr(e) -> (to_string_expr e) ^ ";\n"
      | ABlock(e_list) -> let list_of_strings = List.rev (List.fold_left  (fun list e ->
              (to_string_expr e)::list) [] e_list) in
              String.concat ""  list_of_strings
      | AIf(e, first, second) -> let first_str = to_string_stmt first in 
                                 let second_str = to_string_stmt second in 
                                 "if(" ^  first_str ^ "){" ^  second_str ^ "}"


let to_string_stage (stage : a_stage) : string = 
    let header = "private static void " ^ stage.sname ^ "(){\n" in
    let initial_header = match stage.is_start with
        true -> get_initial_stage_header stage.sname
      | false -> ""
    in let list_of_strings = List.rev (List.fold_left (fun list s ->
        (to_string_stmt s)::list ) [] stage.body) in 
    initial_header ^ header ^ (String.concat "\n" list_of_strings) ^ "}" 
    

let start_gen (sast : a_program) (name : string) =
    make_header name false;
    let list_of_strings = List.rev ( List.fold_left  (fun list s ->
       (to_string_stage s)::list ) [] sast.stages) in 
    let print_out = String.concat ""  list_of_strings ^ "}" in 
    write_out name print_out
      

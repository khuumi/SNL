open Printf
open Sast
open Ast
open Str

(* let get_header = "{\n  public static void main(String args[])\n"
 *)


(*type symbol_table = {
    mutable variables : string list;
}

type environment = {
    global_scope : symbol_table;
    local_scope : symbol_table;
}

let env = { global_scope = { variables = []; };
                  local_scope = { variables = []; }; }
*)
let global_scope = Hashtbl.create 1000  

let write_out (filename : string) (buffer : string) =
    let file = (open_out_gen [Open_creat; Open_wronly;
    Open_text; Open_append] 0o666
    (filename)) in

    fprintf file "%s" buffer;
    close_out file
 

let get_initial_stage_header (start_stage_name : string) =
    "\n public static void main(String args[]){\n" ^start_stage_name ^ "();\n}\n"

let make_header (filename : string) (is_recipe : bool) =
    let strlst = Str.split (Str.regexp "/") filename in
    let tail = List.hd (List.rev strlst) in
    let class_name = String.sub tail 0 ((String.length tail) -5 ) in 
    let scanner = "import java.util.Scanner;\n" in 
    let scanner2 = "private static Scanner input = new Scanner(System.in);" in 
    match is_recipe with
    true -> let header = scanner ^ "public final class " ^ class_name ^ "{"
    ^ scanner2 ^ "\n public static void perform(){\n" in  write_out filename header
  | false -> let header = scanner ^ "public class " ^ class_name ^ "{\n"  ^ scanner2 in write_out
        filename header


let to_string_const (const : a_constant)  : string =
    match const with
       AInt(num, _) -> " new SNLObject("^ (string_of_int num) ^", \"int\")"                 
     | AFloat(fl, _) ->" new SNLObject("^ (string_of_float fl) ^", \"float\")"
     | ABool(b, _) -> " new SNLObject("^  (string_of_bool b) ^", \"bool\")"
     | AString(s, _) ->" new SNLObject(\"" ^ s ^ "\", \"string\")"

let to_string_id (name : string ) (scope : Ast.scope) : string = 
    match scope with 
       Local -> "TODO: local scoped variable"
     | Global -> (match Hashtbl.mem global_scope name with
                       true -> ()
                     | false->  Hashtbl.add global_scope name name);
     name    

let rec to_string_expr (expr : a_expr) : string = 
    match expr with 
      AConstant(const) -> to_string_const const
    | AId(name, scope, _) -> to_string_id name scope 
    | AUnop(op, e, _) ->  to_string_unop e op
    | ABinop(e1, op, e2, t) -> to_string_binop e1 e2 op
    | AAssign(e1, e2) ->to_string_expr e1 ^  "= " ^ to_string_expr e2
    | ANext(s, t) -> s ^ "()"
    | AReturn(e, t) -> "TODO: return"
    | AList(e_list, t) -> to_string_list e_list 
    | AInput(t) -> "new SNLObject(input.nextLine(), \"string\")"
    | ACall(s, e_list, t) -> to_string_call s e_list
    | AAccess(i, e, t) -> "TODO: access" 

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
            "Recipe_" ^  name ^"(" ^ (String.concat ", " list_e_strings) ^ ")" 

 and to_string_list (e_list : a_expr list) : string = 
    let  list_e_strings = List.rev ( List.fold_left 
           (fun list e -> (to_string_expr e)::list ) []  e_list) in 
            "new SNLObject(\"list\", " ^ (String.concat ", " list_e_strings) ^ ")"


               
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
    let global_vars = Hashtbl.fold (fun k v acc ->
                                    "private static SNLObject "
                                    ^ k ^ ";\n" ^ acc) global_scope "" in 
    let print_out = (String.concat "" list_of_strings) ^ global_vars ^ "}" in 
    write_out name print_out      

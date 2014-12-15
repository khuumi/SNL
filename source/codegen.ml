open Printf
open Sast
open Ast

let global_scope = Hashtbl.create 1000;; 
let local_scope = Hashtbl.create 1000;;


let get_initial_stage_header (start_stage_name : string)
                             (is_recipe : bool)
                             (formals : string list) =
  match is_recipe with
    true -> let initial = "\n \tpublic static SNLObject perform(" in 
            let list_of_args = List.map 
                                 (fun name -> "SNLObject " ^ name ^ "_arg")
                                 formals in
            let perform_args =  (String.concat ", " list_of_args) ^ "){\n" in
            let args_in_body = List.map 
                                 (fun name -> name ^ " = new SNLObject(" ^
                                                name ^ "_arg);\n") formals in 
            let constructs = (String.concat "" args_in_body) in 
            initial ^ perform_args ^ constructs ^ start_stage_name ^
              "();\nreturn ret;\n}\n" 
  | false -> "\n public static void main(String args[]){\n" ^ start_stage_name ^
               "();\n}\n"


let to_string_const (const : a_constant) : string =
  match const with
    AInt(num, _) -> " new SNLObject(" ^ (string_of_int num) ^ ", \"int\")"
  | AFloat(fl, _) ->" new SNLObject(" ^ (string_of_float fl) ^ ", \"float\")"
  | ABool(b, _) -> " new SNLObject(" ^  (string_of_bool b) ^ ", \"bool\")"
  | AString(s, _) -> " new SNLObject(\"" ^ s ^ "\", \"string\")"


let to_string_id (name : string) (scope : Ast.scope) : string = 
  match scope with 
      Local -> (match Hashtbl.mem local_scope name with
                     true -> name
                   | false->  Hashtbl.add local_scope name name; "SNLObject " ^ name)
    | Global -> (match Hashtbl.mem global_scope name with
                     true -> ()
                   | false->  Hashtbl.add global_scope name name);
                name    


let rec to_string_expr (expr : a_expr) : string = 
  match expr with 
    AConstant(const) -> to_string_const const
  | AId(name, scope, _) -> to_string_id name scope 
  | AUnop(op, e, _) ->  to_string_unop e op
  | ABinop(e1, op, e2, _) -> to_string_binop e1 e2 op
  | AAssign(e1, e2) -> to_string_expr e1 ^  "= " ^ to_string_expr e2
  | ANext(s, _) -> s ^ "()"
  | AReturn(e, _) -> "ret = " ^ (to_string_expr e) ^ ";\n" ^ "return" 
  | AList(e_list, _) -> to_string_list e_list 
  | AInput(t) -> "new SNLObject(input.nextLine(), \"string\")"
  | ACall(s, e_list, _) -> to_string_call s e_list
  | AAccess(index_e, e, _) -> (to_string_expr e) ^
                                ".getArr()[" ^
                                  (to_string_expr index_e) ^
                                    ".getInt()]"


and to_string_unop (e : a_expr) (op : Ast.op) : string = 
  let string_op = 
    match op with 
      Negate -> "neg"
    | Not -> "not" 
    | _ -> "Error" in 
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
    | Or -> "or"
    | _ -> "ERROR" in 
  (to_string_expr e1) ^ "." ^ string_op ^ "(" ^ (to_string_expr e2) ^ ")"


and to_string_call (name : string) (e_list : a_expr list) : string = 
  match name with 
    "show" -> let list_e_strings = List.rev (List.fold_left   
                                               (fun list e ->
                                                (to_string_expr e) :: list)
                                               []
                                               e_list) in
              "System.out.println(" ^ (String.concat " + " list_e_strings) ^")"
  | _ -> let list_e_strings = List.rev (List.fold_left 
                                          (fun list e ->
                                           (to_string_expr e) :: list)
                                          []
                                          e_list) in 
         "Recipe_" ^ name ^ ".perform(" ^
           (String.concat ", " list_e_strings) ^ ")"


and to_string_list (e_list : a_expr list) : string = 
  let list_e_strings = List.rev (List.fold_left 
                                   (fun list e ->
                                    (to_string_expr e) :: list)
                                   []
                                   e_list) in
  "new SNLObject(\"list\", " ^ (String.concat ", " list_e_strings) ^ ")"


let rec to_string_stmt (statement : a_stmt) = 
  match statement with 
    AExpr(e) -> (to_string_expr e) ^ ";\n"
  | ABlock(e_list) -> 
     let list_of_strings = List.rev (List.fold_left 
                                       (fun list e ->
                                        (to_string_expr e ^";\n") :: list)
                                       []
                                       e_list) in
     String.concat "" list_of_strings
  | AIf(e, first, second) -> let expr_str = (to_string_expr e ) in 
                             let first_str = to_string_stmt first in 
                             let second_str = to_string_stmt second in 
                             "if(" ^  expr_str ^ ".getBool())\n{" ^  first_str ^
                               "}\n" ^ "else{" ^second_str ^ "}\n"


let to_string_stage (stage : a_stage)
                    (is_recipe : bool)
                    (formals : string list) : string = 
  Hashtbl.clear local_scope;
  let header = "private static void " ^ stage.sname ^ "(){\n" in
  let initial_header =
    if stage.is_start
    then get_initial_stage_header stage.sname is_recipe formals
    else ""
  in let list_of_strings = List.rev (List.fold_left
                                       (fun list s ->
                                        (to_string_stmt s) :: list)
                                       []
                                       stage.body) in 
     initial_header ^ header ^ (String.concat "\n" list_of_strings) ^ "}" 


let to_string_stages (stages : a_stage list)
                     (is_recipe : bool) 
                     (formals : string list) = 
  let list_of_strings = List.rev
                          (List.fold_left 
                             (fun list s ->
                              (to_string_stage s is_recipe formals) :: list)
                             []
                             stages) in 
  let global_vars = Hashtbl.fold (fun k v acc ->
                                  "private static SNLObject "
                                  ^ k ^ ";\n" ^ acc) global_scope "" in 
  (String.concat "" list_of_strings) ^ global_vars ^ "}"


(* name should be the file name of the snl file or recipe
   without any extensions. *)
let make_header (name : string) (is_recipe : bool) : string =
  let scanner = "import java.util.Scanner;\n" in 
  let scanner2 = "\tprivate static Scanner input = new Scanner(System.in);" in 
  if is_recipe
  then scanner ^ "public final class " ^ "Recipe_" ^ name ^ "{\n" ^
         "\tprivate static SNLObject ret;\n" ^ scanner2
  else scanner ^ "public class " ^ name ^ "{\n" ^ scanner2


let gen_main (stages : a_stage list) (name : string) : string =
  make_header name false ^ to_string_stages stages false []


let gen_recipe (recipe : a_recipe) : string =
  Hashtbl.clear global_scope;
  List.iter (fun formal -> Hashtbl.add global_scope formal formal) recipe.formals;
  make_header recipe.rname true ^
    to_string_stages recipe.body true recipe.formals

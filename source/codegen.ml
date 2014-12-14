open Printf
open Sast
open Ast

let global_scope = Hashtbl.create 1000;;  

let write_out (filename : string) (buffer : string) =
    let file = (open_out_gen [Open_creat; Open_wronly;
    Open_text; Open_append] 0o666
    (filename)) in

    fprintf file "%s" buffer;
    close_out file
 

let get_initial_stage_header (start_stage_name : string) (is_recipe : bool) =
        match is_recipe with 
       true -> ""
     | false ->  "\n public static void main(String args[]){\n" ^start_stage_name ^
    "();\n}\n"


let make_header (filename : string) (is_recipe : bool) =
    let scanner = "import java.util.Scanner;\n" in 
    let scanner2 = "\tprivate static Scanner input = new Scanner(System.in);" in 

    match is_recipe with 
    true -> let header = scanner ^ "public final class " ^ filename ^ "{\n"
    ^ "\tprivate static SNLObject ret;\n" ^ scanner2 ^ 
    "\n \tpublic static void perform(" in 
    write_out ("Recipe_" ^filename ^".java") header
    | false -> let class_name = String.sub filename 0 ((String.length filename) -5 ) in 
    let header = scanner ^ "public class " ^ class_name ^ "{\n"  ^ scanner2 in write_out
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


let to_string_stage (stage : a_stage) (is_recipe : bool) : string = 
    let header = "private static void " ^ stage.sname ^ "(){\n" in
    let initial_header = match stage.is_start with
        true -> get_initial_stage_header stage.sname is_recipe
      | false -> ""
    in let list_of_strings = List.rev (List.fold_left (fun list s ->
        (to_string_stmt s)::list ) [] stage.body) in 
    initial_header ^ header ^ (String.concat "\n" list_of_strings) ^ "}" 


let to_string_stages (stages : a_stage list) (is_recipe : bool)  = 
  let list_of_strings = List.rev ( List.fold_left  (fun list s ->
      (to_string_stage s is_recipe)::list ) [] stages) in 
    let global_vars = Hashtbl.fold (fun k v acc ->
                                    "private static SNLObject "
                                    ^ k ^ ";\n" ^ acc) global_scope "" in 
   (String.concat "" list_of_strings) ^ global_vars ^ "}" 
    

let print_recipe (recipe : a_recipe) = 
    make_header recipe.rname true; 
    let list_of_args = 
        List.map (fun name -> "SNLObject " ^ name ^ "_arg") recipe.formals
    in let perform_args =  (String.concat ", " list_of_args) ^ "){\n" in 
    let args_in_body = List.map (fun name -> name ^ " = new SNLObject(" ^ name
    ^ "_arg);\n") recipe.formals in 
    let constructs = (String.concat "" args_in_body) in 
    let stages_string = to_string_stages recipe.body true in 
    let starting_stage = "lol" in 
    write_out ("Recipe_" ^ recipe.rname ^ ".java") (perform_args ^ constructs  ^ starting_stage
    ^"();\nreturn ret;\n}\n" ^ stages_string) 


    (* first do a List.map and add SNLObject '_arg' to every argument name in
     * formals. Then assign copy constructur to the actual argument name and
     * then add that to the hashtable -- then call first stage and return ret. 
     * After that do all the normal stage stuff -- remember to clear the
     * hashtable for each recipe *)

    
let start_gen (sast : a_program) (name : string) =
    (* Make the header for the recipe *)
    (* Remember to clear after each recipe *)
    
    List.iter (fun recipe -> print_recipe recipe; Hashtbl.clear global_scope )
    sast.recipes;
    make_header name false;
    write_out name (to_string_stages sast.stages false)

open Ast
open Printf
open Sast


let global_scope = Hashtbl.create 1000;;
let local_scope = Hashtbl.create 1000;;


let get_initial_stage_header (start_stage_name : string)
                             (is_recipe : bool)
                             (formals : string list) =
  if is_recipe
  then let initial = "\n \tpublic SNLObject perform(" in
       let list_of_args = List.map
                            (fun name -> "SNLObject " ^ name ^ "_arg")
                            formals in
       let perform_args =  (String.concat ", " list_of_args) ^ "){\n" in
       let args_in_body = List.map
                            (fun name -> name ^ " = new SNLObject(" ^
                                           name ^ "_arg);\n") formals in
       let constructs = (String.concat "" args_in_body) in
       initial ^ perform_args ^ constructs ^"s_" ^ start_stage_name ^
         "();\nreturn ret;\n}\n"
  else "\n public static void main(String args[]){\ns_" ^ start_stage_name ^
         "();\n}\n"


let to_string_const (const : a_constant) : string =
  match const with
    AInt(num, _) -> " new SNLObject(" ^ (string_of_int num) ^ ")"
  | AFloat(fl, _) ->" new SNLObject(" ^ (string_of_float fl) ^ ")"
  | ABool(b, _) -> " new SNLObject(" ^  (string_of_bool b) ^ ")"
  | AString(s, _) -> " new SNLObject(\"" ^ s ^ "\")"


let to_string_id (name : string) (scope : Ast.scope) : string =
  match scope with
    Local -> (match Hashtbl.mem local_scope name with
                true -> name
              | false->  Hashtbl.add local_scope name name;
                         "SNLObject " ^ name)
  | Global -> (match Hashtbl.mem global_scope name with
                 true -> name
               | false-> Hashtbl.add global_scope name name; name)


let rec to_string_expr (expr : a_expr) : string =
  match expr with
    AConstant(const) -> to_string_const const
  | AId(name, scope, _) -> to_string_id name scope
  | AUnop(op, e, _) ->  to_string_unop e op
  | ABinop(e1, op, e2, _) -> to_string_binop e1 e2 op
  | AAssign(e1, e2) -> to_string_expr e1 ^  "= " ^ to_string_expr e2
  | ANext(s, _) -> "s_" ^ s ^ "();\nreturn"
  | AReturn(e, _) -> "ret = " ^ (to_string_expr e) ^ ";\n" ^ "return"
  | AList(e_list, _) -> to_string_list e_list
  | AInput(t) -> "new SNLObject(input.nextLine())"
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
  | "remove" -> let lst = to_string_expr (List.nth e_list 0) in
                let index = to_string_expr (List.nth e_list 1) in
                lst ^ ".remove(" ^ index ^ ")"
  | "insert" -> let lst = to_string_expr (List.nth e_list 0) in
                let item_to_add = (to_string_expr (List.nth e_list 1)) in
                let index = to_string_expr (List.nth e_list 2) in
                lst ^ ".insert(" ^ index ^ ", " ^ item_to_add ^ ")"
  | "append" -> let lst = to_string_expr (List.nth e_list 0) in
                let item_to_add = to_string_expr (List.nth e_list 1) in
                lst ^ ".app(" ^ item_to_add ^ ")"
  | "length" -> let lst = to_string_expr (List.nth e_list 0) in
                lst ^ ".length()"
  | "word_to_number" -> let word = to_string_expr (List.nth e_list 0) in
                        word ^ ".word_to_number()"
  | "number_to_word" -> let word = to_string_expr (List.nth e_list 0) in
                        word ^ ".number_to_word()"
  | _ -> let list_e_strings = List.rev (List.fold_left
                                          (fun list e ->
                                           (to_string_expr e) :: list)
                                          []
                                          e_list) in
         "new Recipe_" ^ name ^ "().perform(" ^
           (String.concat ", " list_e_strings) ^ ")"


and to_string_list (e_list : a_expr list) : string =
  let list_e_strings = List.rev (List.fold_left
                                   (fun list e ->
                                    (to_string_expr e) :: list)
                                   []
                                   e_list) in
  "new SNLObject(" ^ (String.concat ", " list_e_strings) ^ ")"


let rec to_string_stmt (statement : a_stmt) =
  match statement with
    AExpr(e) -> (to_string_expr e) ^ ";\n"
  | ABlock(s_list) ->
     let list_of_strings = List.rev (List.fold_left
                                       (fun list e ->
                                        (to_string_stmt e) :: list)
                                       []
                                       s_list) in
     String.concat "" list_of_strings
  | AIf(e, first, second) -> let expr_str = (to_string_expr e) in
                             let first_str = to_string_stmt first in
                             let second_str = to_string_stmt second in
                             "if(" ^  expr_str ^ ".getBool())\n{" ^  first_str ^
                               "}\n" ^ "else{" ^second_str ^ "}\n"


let to_string_stage (stage : a_stage)
                    (is_recipe : bool)
                    (formals : string list) : string =
  Hashtbl.clear local_scope;
  let header =
    if is_recipe then "private void s_" ^ stage.sname ^ "(){\n"
    else "private static void s_" ^ stage.sname ^ "(){\n"
  in
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
  let global_vars =
    if is_recipe then Hashtbl.fold (fun k v acc ->
                                    "private SNLObject "
                                    ^ k ^ ";\n" ^ acc) global_scope ""
    else Hashtbl.fold (fun k v acc ->
                       "private static SNLObject "
                       ^ k ^ ";\n" ^ acc) global_scope ""
  in
  (String.concat "" list_of_strings) ^ global_vars ^ "}"


(* name should be the file name of the snl file or recipe
   without any extensions. *)
let make_header (name : string) (is_recipe : bool) : string =
  let scanner = "import java.util.Scanner;\n" in
  if is_recipe
  then let scanner2 = "\tprivate Scanner input = new Scanner(System.in);" in 
       scanner ^ "public class " ^ "Recipe_" ^ name ^ "{\n" ^
         "\tprivate SNLObject ret;\n" ^ scanner2 ^ "\npublic Recipe_" ^ name ^
           "(){}\n"
  else let scanner2 = "\tprivate static Scanner input = " ^
                        "new Scanner(System.in);" in
       scanner ^ "public class " ^ name ^ "{\n" ^ scanner2


let gen_main (stages : a_stage list) (name : string) : string =
  make_header name false ^ to_string_stages stages false []


let gen_recipe (recipe : a_recipe) : string =
  Hashtbl.clear global_scope;
  List.iter (fun formal -> Hashtbl.add global_scope formal formal)
            recipe.formals;
  make_header recipe.rname true ^
    to_string_stages recipe.body true recipe.formals

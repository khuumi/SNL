type op =
  Add | Sub | Mult | Div | Negate |
  Equal | Neq | Lt | Leq | Gt | Geq |
  And | Or | Not

type scope = Local | Global

type constant =
    Int of int
  | Float of float
  | Bool of bool
  | String of string

type expr =
    Constant of constant
  | Id of string * scope
  | Unop of op * expr
  | Binop of expr * op * expr
  | Assign of expr * expr
  | Call of string * expr list
  | List of expr list
  | Return of expr
  | Next of string
  | Input

type stmt =
    Expr of expr
  | Block of expr list
  | If of expr * stmt * stmt

type stage = {
    sname: string;        (* Name of the stage. *)
    locals: string list;  (* Locally defined variables. *)
    body: stmt list;      (* The statements that comprise the stage. *)
    is_start: bool;       (* Whether the stage is a start stage. *)
}

type recipe = {
    rname: string;         (* Name of the recipe. *)
    formals: string list;  (* Formal argument names. *)
    body: stage list;      (* Stages in the recipe's scope. *)
    globals: string list;  (* Variables global inside the recipe. *)
}

type program = {
    recipes: recipe list;
    stages: stage list;
}


(* Low-level AST printing, to help debug the structure. *)

let op_s = function
    Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Negate -> "Negate"
  | Equal -> "Equal"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Leq -> "Leq"
  | Gt -> "Gt"
  | Geq -> "Geq"
  | And -> "And"
  | Or -> "Or"
  | Not -> "Not"

let constant_s = function
    Int(i) -> "Int " ^ string_of_int i
  | Float(f) -> "Float " ^ string_of_float f
  | Bool(b) -> "Bool " ^ string_of_bool b
  | String(s) -> "String " ^ s

let rec expr_s = function
    Constant(c) -> constant_s c
  | Id(str, scope) -> "Id " ^
                        (match scope with Local -> "Local "
                                        | Global -> "Global ") ^
                          str
  | Unop(o, e) -> "Unop " ^ (op_s o) ^ " (" ^ expr_s e ^ ")"
  | Binop(e1, o, e2) -> "Binop (" ^ expr_s e1 ^ ") " ^
                          (op_s o) ^
                            " (" ^ expr_s e2 ^ ")"
  | Assign(v, e) -> "Assign (" ^ expr_s v ^ ") (" ^ expr_s e ^ ")"
  | Call(f, es) -> "Call " ^ f ^ " [" ^
                     String.concat ", " (List.map
                                           (fun e -> "(" ^ expr_s e ^ ")")
                                           es) ^
                       "]"
  | List(es) -> "List [" ^
                  String.concat ", " (List.map
                                        (fun e -> "(" ^ expr_s e ^ ")")
                                        es) ^
                    "]"
  | Return(e) -> "Return (" ^ expr_s e ^ ")"
  | Next(s) -> "Next " ^ s
  | Input -> "input"

let rec stmt_s = function
    Expr(e) -> "Expr (" ^ expr_s e ^ ")"
  | Block(es) -> "Block [" ^
                   String.concat ",\n"
                                 (List.map (fun e -> "(" ^ expr_s e ^ ")") es) ^
                     "]"
  | If(e, s1, s2) -> "If (" ^ expr_s e ^ ") (" ^ stmt_s s1 ^ ") (" ^
                       stmt_s s2 ^ ")"

let stage_s s =
  "{ sname = \"" ^ s.sname ^ "\"\n" ^
    "   is_start = " ^ string_of_bool s.is_start ^ "\n" ^
      "   locals = [" ^ String.concat ", " s.locals ^ "]\n" ^
        "   body = [" ^ String.concat ",\n" (List.map stmt_s s.body) ^
          "]}\n"

let recipe_s r =
  "{ rname = \"" ^ r.rname ^ "\"\n" ^
    "   formals = [" ^ String.concat ", " r.formals ^ "]\n" ^
      "   globals = [" ^ String.concat ", " r.globals ^ "]\n" ^
        "   body = [" ^ String.concat ",\n" (List.map stage_s r.body) ^
          "]}\n"

let program_s prog =
  "recipes = [" ^ String.concat ",\n" (List.map recipe_s prog.recipes) ^
    "],\n" ^
    "stages = [" ^  String.concat ",\n" (List.map stage_s prog.stages) ^ "]"


(* (\* "Pretty printed" version of the AST, meant to generate a MicroC program *)
(*    from the AST.  These functions are only for pretty-printing (the -a flag) *)
(*    the AST and can be removed. *\) *)

(* let rec string_of_expr = function *)
(*     Literal(l) -> string_of_int l *)
(*   | Id(s) -> s *)
(*   | Binop(e1, o, e2) -> *)
(*       string_of_expr e1 ^ " " ^ *)
(*       (match o with *)
(* 	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" *)
(*       | Equal -> "==" | Neq -> "!=" *)
(*       | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^ *)
(*       string_of_expr e2 *)
(*   | Assign(v, e) -> v ^ " = " ^ string_of_expr e *)
(*   | Call(f, el) -> *)
(*       f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")" *)
(*   | Noexpr -> "" *)

(* let rec string_of_stmt = function *)
(*     Block(stmts) -> *)
(*       "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n" *)
(*   | Expr(expr) -> string_of_expr expr ^ ";\n"; *)
(*   | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"; *)
(*   | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s *)
(*   | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ *)
(*       string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2 *)
(*   | For(e1, e2, e3, s) -> *)
(*       "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ *)
(*       string_of_expr e3  ^ ") " ^ string_of_stmt s *)
(*   | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)

(* let string_of_vdecl id = "int " ^ id ^ ";\n" *)

(* let string_of_fdecl fdecl = *)
(*   fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^ *)
(*   String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
(*   String.concat "" (List.map string_of_stmt fdecl.body) ^ *)
(*   "}\n" *)

(* let string_of_program (vars, funcs) = *)
(*   String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^ *)
(*   String.concat "\n" (List.map string_of_fdecl funcs) *)

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
  | Access of expr * expr

type stmt =
    Expr of expr
  | Block of stmt list
  | If of expr * stmt * stmt

type stage = {
    sname: string;        (* Name of the stage. *)
    body: stmt list;      (* The statements that comprise the stage. *)
    is_start: bool;       (* Whether the stage is a start stage. *)
}

type recipe = {
    rname: string;         (* Name of the recipe. *)
    formals: string list;  (* Formal argument names. *)
    body: stage list;      (* Stages in the recipe's scope. *)
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
  | Access(i, l) -> "Access " ^ (expr_s i) ^ " of " ^ (expr_s l)

let rec stmt_s = function
    Expr(e) -> "Expr (" ^ expr_s e ^ ")"
  | Block(ss) -> "Block [" ^
                   String.concat ",\n"
                                 (List.map (fun s -> "(" ^ stmt_s s ^ ")") ss) ^
                     "]"
  | If(e, s1, s2) -> "If (" ^ expr_s e ^ ") (" ^ stmt_s s1 ^ ") (" ^
                       stmt_s s2 ^ ")"

let stage_s s =
  "{ sname = \"" ^ s.sname ^ "\"\n" ^
    "   is_start = " ^ string_of_bool s.is_start ^ "\n" ^
      "   body = [" ^ String.concat ",\n" (List.map stmt_s s.body) ^
        "]}\n"

let recipe_s r =
  "{ rname = \"" ^ r.rname ^ "\"\n" ^
    "   formals = [" ^ String.concat ", " r.formals ^ "]\n" ^
      "   body = [" ^ String.concat ",\n" (List.map stage_s r.body) ^
        "]}\n"

let program_s prog =
  "recipes = [" ^ String.concat ",\n" (List.map recipe_s prog.recipes) ^
    "],\n" ^
    "stages = [" ^  String.concat ",\n" (List.map stage_s prog.stages) ^ "]"

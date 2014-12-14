
(* The basic types used in annotation *)
type t =
    TInt
  | TFloat
  | TBool
  | TString
  | TList of t array
  | TOCamlString
  | TUnknown

type a_constant =
    AInt of int * t
  | AFloat of float * t
  | ABool of bool * t
  | AString of string * t

type a_expr =
    AConstant of a_constant
  | AId of string * Ast.scope * t
  | AUnop of Ast.op * a_expr * t
  | ABinop of a_expr * Ast.op * a_expr * t
  | AAssign of a_expr * a_expr
  | ANext of string * t
  | AReturn of a_expr * t
  | AList of a_expr list * t
  | AInput of t
  | ACall of string * a_expr list * t
  | AAccess of a_expr * a_expr * t

type a_stmt =
    AExpr of a_expr
  | ABlock of a_expr list
  | AIf of a_expr * a_stmt * a_stmt

type a_stage = {
    sname: string;        (* Name of the stage. *)
    body: a_stmt list;    (* The annotated statements in the stage. *)
    is_start: bool;       (* Whether the stage is a start stage. *)
}

type a_recipe = {
    rname: string;         (* Name of the recipe. *)
    formals: string list;  (* Formal argument names. *)
    body: a_stage list;      (* Stages in the recipe's scope. *)
}

type a_program = {
    recipes: a_recipe list;
    stages: a_stage list;
}

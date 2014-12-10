
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
  | AAssign of a_expr
  | ANext of string * t
  | AReturn of a_expr * t
  | AList of a_expr list * t
  | AInput of t
  | ACall of string * a_expr list * t
  | AAccess of int * a_expr * t

(* type t_program = {
    t_recipes = t_recipe list
    t_stage  = t_stage list
} *)

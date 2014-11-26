
(* The basic types used in annotation *)
type t =
    TInt
  | TFloat
  | TBool
  | TString

type a_constant =
    AInt of int * t
  | AFloat of float * t
  | ABool of bool * t
  | AString of string * t


type a_expr =
    AConstant of a_constant
  | AId of string * Ast.scope * t
  | AUnop of Ast.op * a_expr
  | ABinop of a_expr * Ast.op * a_expr * t





type t_program = {
    t_recipes = t_recipe list
    t_stage  = t_stage list
}
open Ast
open Sast

(* A symbol table wich includes a parent symbol table
   and variables which are tuples of stings and Sast types *)
type symbol_table = {
    mutable variables : (string * Sast.t) list;
}

type environment = {
    mutable function_return_type : Sast.t option;
    global_scope : symbol_table;
    local_scope : symbol_table;
}

let type_of_const (ac : Sast.a_constant) : Sast.t =
  match ac with
  | AInt(_, t) -> t
  | AFloat(_, t) -> t
  | ABool(_, t) -> t
  | AString(_, t) -> t

let rec type_of (ae : Sast.a_expr) : Sast.t =
  match ae with
  | AConstant(const) -> type_of_const const
  | AId(_,_, t) -> t
  | AUnop(_, _, t) -> t
  | ABinop(_, _, _, t) -> t
  | AAssign(expr) -> type_of expr


let rec find_variable (scope : symbol_table) (name : string) : Sast.t option =
  try
    let (_, typ) = List.find (fun (s, _) -> s = name ) scope.variables in
    Some(typ)
  with Not_found -> None


(* Check to see if paran is important or not *)
let mutate_or_add (scope : symbol_table) (name : string) (new_type : Sast.t) =
  let typ = find_variable scope name in
  match typ with
  | Some(t) ->
    (* filter name, t out of symbol_table.variables *)
    scope.variables <- (name, new_type) :: (List.filter (fun (s, _) -> s <> name) scope.variables)
  | None ->
    scope.variables <- (name, new_type) :: scope.variables

let require_int_float (expr : a_expr) (err : string) : int =
  match type_of expr with
  | TInt -> 0
  | TFloat -> 1
  | _ -> failwith err


let require_bool (expr : a_expr) (err : string) : int =
  match type_of expr with
  | TBool -> 0
  | _ -> failwith err


let weak_eq_type (t1 : Sast.t) (t2 : Sast.t) =
  true

(*   let scope.variables =
    scope.variables ::
 *)

  (*
    Check to see if e1 is actually an ID, then annotate e2 and add this
    assignment to the enviroment ( check the scope of the ID to add it to
    the correct one)

   *)



(* Loads of useful utilities -- let me build them out
   later if I need them  *)

(* First we annotate  *)

(* let rec annotate_expr (e : Ast.expr ) (env : environment)
 *)

(* what is the : Sast.t_expr actually doing? *)

let annotate_const (c : Ast.constant) : Sast.a_expr =
    match c with
  | Int(n) -> AConstant(AInt(n, TInt))
  | Float(f) -> AConstant(AFloat(f, TFloat))
  | Bool(b) -> AConstant(ABool(b, TBool))
  | String(s) -> AConstant(AString(s, TString))

let rec annotate_expr (e : Ast.expr) (env : environment) : Sast.a_expr =
    match e with
  | Constant(c) -> annotate_const c
  | Id(i, s) ->
      let typ =
        match s with
        | Local -> find_variable env.local_scope
        | Global -> find_variable env.global_scope
      in AId(i, s, TInt)
      (* TODO: FIX THIS WHY IS THIS NOT WORKING *)
      (* (match typ with
        | Some(x) -> AId(i, s, x)
        | None -> fails_with "unrecognized identifier " ^ i ^ ".") *)
  | Unop(op, e1) ->
      let ae1 = annotate_expr e1 env in
      AUnop(op, ae1, type_of ae1)
  | Binop(e1, op, e2) ->
      let ae1 = annotate_expr e1 env
      and ae2 = annotate_expr e2 env in

      (match op with
        | Add | Sub | Mult | Div
        | Neq | Lt | Leq | Gt | Geq ->
          (let a = require_int_float ae1 "left operand must be either integer or float" and
            b = require_int_float ae2 "right operand must be either integer or float" in
             if a + b = 0 then
             ABinop(ae1, op, ae2, TInt)
            else ABinop(ae1, op, ae2, TFloat))
        | And | Or ->
          ignore (require_bool ae1 "left operand must be a bool");
          ignore (require_bool ae2 "right operand must be a bool");
          ABinop(ae1, op, ae2, TBool)
        | _ ->
          if not (weak_eq_type (type_of ae1) (type_of ae2)) then
          failwith "Type mismatch in comparison" else
          ABinop(ae1, op, ae2, TBool))
    | Assign(e1, e2) ->
        let str, ret_value, scope =
          match e1 with
          | Id(str2, scope) -> str2, (annotate_expr e2 env), scope
          | _ -> failwith "PLACEHOLDER"
        in match scope with
        (* TODO: Get the actual type to pass into mutate_or_add *)
          | Local -> mutate_or_add env.local_scope str TInt; AAssign(ret_value)
          | Global -> mutate_or_add env.global_scope str TInt; AAssign(ret_value)
        (* ADD: add the case where the left is an array access *)

(* let annotate_prog (p : Ast.program ) : Sast.t_program =
    let env  = create_env() in
    annotate_recipes p env
    annotate_stages p env


 *)



open Ast


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

let rec find_variable (scope : symbol_table) (name : String) : Sast.t option =
  try
    let (_, typ) = List.find (fun (s, _) -> s = name ) scope.variables in
    Some(typ)
  with Not_Found -> None


(* Loads of useful utilities -- let me build them out
   later if I need them  *)

(* First we annotate  *)

(* let rec annotate_expr (e : Ast.expr ) (env : environment)
 *)

(* what is the : Sast.t_expr actually doing? *)

let annotate_const (c : Ast.constant) : Sast.a_constant =
    match c with
  | Int(n)    -> AInt(n, TInt)
  | Float(f)  -> AFloat(f, TFloat)
  | Bool(b)   -> ABool(b, TBool)
  | String(s) -> AString(s, TString)

and rec annotate_expr (e : Ast.expr) (env : environment) : Sast.a_expr =
    match e with
  | Constant(c)   -> annotate_const c
  | Id(i, s)      ->
      let typ =
        match s with
        | Local -> find_variable env.local_scope
        | Global -> find_variable env.global_scope
      in (match typ with
        | Some(x)   -> AId(i, s, x)
        | None      -> fails_with("unrecognized identifier " ^ i ^ ".")
      )
  | Unop(e1, op)  ->
      let ae1 = annotate_expr e1 env in
      AUnop(ae1, TBool)
  | Binop(e1, op, e2) ->
      let ae1 = annotate_expr e1 env
      and ae2 = annotate_expr e2 env in

      let _, t1 = e1
      and _, t2 = e2 in

      (* This if the operation is anything besides =, != it requires that both
         operands be integers or floats *)
      if op <> Ast.op.Equal && op <> Ast.op.Neq &&
         op <> Ast.op.And && op <> Ast.op.Or then
        (let a = require_int_float e1 "left operand must be either integer or float" and
         let b = require_int_float e2 "right operand must be either integer or float" in
          if a + b = 0 then
          ABinop(ae1, op, ae2, TInt)
          else ABinop(ae1, op, ae2, TFloat))


      else if op = Ast.op.And || op = Ast.op.Or then
          (require_bool e1 "left operand must be a bool";
           require_bool e2 "right operand must be a bool";
          ABinop(ae1, op, ae2, TBool) )
      else
        if not (weak_eq_type t1 t2) then
          fails_with "Type mismatch in comparison" else
          ABinop(ae1, op, ae2, TBool)




let annotate_prog (p : Ast.program ) : Sast.t_program =
    let env  = create_env() in
    annotate_recipes p env
    annotate_stages p env






open Ast
open Sast

(* A symbol table wich includes a parent symbol table
   and variables which are tuples of stings and Sast types *)
type symbol_table = {
    mutable variables : (string * Sast.t) list;
}


type environment = {
    global_scope : symbol_table;
    local_scope : symbol_table;
}


let type_of_const (ac : Sast.a_constant) : Sast.t =
  match ac with
    AInt(_, t) -> t
  | AFloat(_, t) -> t
  | ABool(_, t) -> t
  | AString(_, t) -> t


let rec type_of (ae : Sast.a_expr) : Sast.t =
  match ae with
    AConstant(const) -> type_of_const const
  | AId(_, _, t) -> t
  | AUnop(_, _, t) -> t
  | ABinop(_, _, _, t) -> t
  | AAssign(e1, e2) -> type_of e2
  | ANext(_, t) -> t
  | AReturn(_, t) -> t
  | AList(_, t) -> t
  | AInput(t) -> t
  | ACall(_, _, t) -> t
  | AAccess(_, _, t) -> t

let jcheck_recipe (s : string) (ae_lst : Sast.a_expr list) =
  s


let find_variable_type (env : environment) (id : Ast.expr) :
      Sast.t option =
  try
    let (_, typ) = match id with
        Id(name, Local) -> List.find
                             (fun (s, _) -> s = name)
                             env.local_scope.variables
      | Id(name, Global) -> List.find
                              (fun (s, _) -> s = name)
                              env.global_scope.variables
      | _ -> failwith "Error using find_variable_type"
    in
    Some(typ)
  with Not_found -> None


(* Check to see if param is important or not *)
let mutate_or_add (env : environment) (id : Ast.expr) (new_type : Sast.t) =
  let typ = find_variable_type env id in
  let name, scope = match id with
      Id(i, Local) -> i, env.local_scope
    | Id(i, Global) -> i, env.global_scope
    | _ -> failwith "Error using mutate_or_add"
  in
  match typ with
    Some(t) ->
    (* filter name, t out of symbol_table.variables *)
    scope.variables <-
      (name, new_type) :: (List.filter (fun (s, _) -> s <> name)
                                       scope.variables)
  | None ->
     scope.variables <- (name, new_type) :: scope.variables


let require_int_float (expr : a_expr) (err : string) : int =
  match type_of expr with
    TInt -> 0
  | TFloat -> 1
  | _ -> failwith err


let require_bool (expr : a_expr) (err : string) : bool =
  match type_of expr with
    TBool -> true
  | _ -> failwith err


let annotate_const (c : Ast.constant) : Sast.a_expr =
  match c with
    Int(n) -> AConstant(AInt(n, TInt))
  | Float(f) -> AConstant(AFloat(f, TFloat))
  | Bool(b) -> AConstant(ABool(b, TBool))
  | String(s) -> AConstant(AString(s, TString))


let rec annotate_expr (e : Ast.expr) (env : environment) : Sast.a_expr =
  match e with
    Constant(c) -> annotate_const c
  | Id(i, s) ->
     (match find_variable_type env e with
      | Some(x) -> AId(i, s, x)
      | None -> failwith ("unrecognized identifier " ^ i ^ "."))
  | Unop(op, e1) ->
     let ae1 = annotate_expr e1 env in
     AUnop(op, ae1, type_of ae1)
  | Binop(e1, op, e2) ->
     let ae1 = annotate_expr e1 env
     and ae2 = annotate_expr e2 env in
     (match op with
      | Add | Sub | Mult | Div
      | Lt | Leq | Gt | Geq ->
        (let a = require_int_float
                   ae1
                   "left operand must be either integer or float" and
             b = require_int_float
                   ae2
                   "right operand must be either integer or float" in
         if a + b = 0 then
           ABinop(ae1, op, ae2, TInt)
         else ABinop(ae1, op, ae2, TFloat))
      | And | Or ->
               ignore (require_bool ae1 "left operand must be a bool");
               ignore (require_bool ae2 "right operand must be a bool");
               ABinop(ae1, op, ae2, TBool)
      | _ -> (* Equal | Neq *)
         if not (type_of ae1 = type_of ae2) then
           failwith "Type mismatch in comparison" else
           ABinop(ae1, op, ae2, TBool))
  | Assign(e1, e2) ->
     (match e1 with
      | Id(str, scope) -> let ae2 = annotate_expr e2 env in
                          mutate_or_add env e1 (type_of ae2); 
                          let ae1 = annotate_expr e1 env in  
                          AAssign(ae1, ae2)
      | Access(e, id) -> let ae2 = annotate_expr e2 env in
                         let ae1 = annotate_expr e1 env in 
                         (match find_variable_type env id with
                          | Some(TList) -> AAssign(ae1, ae2)
                          | _ -> failwith "Variable not found")
      | _ -> failwith "Invalid assignment operation")
  | Next(s) -> ANext(s, TOCamlString)
  | Return(e) -> let ae = annotate_expr e env in
                 AReturn(ae, type_of ae)
  | List(e_list) -> let ae_list = List.map
                                    (fun e -> annotate_expr e env)
                                    e_list in
                    AList(ae_list, TList)
  | Input -> AInput(TString)
  | Call(s, e_list) -> let ae_list = List.map
                                       (fun e-> annotate_expr e env)
                                       e_list in
                       let jcheck = jcheck_recipe s ae_list in
                       ACall(s, ae_list, TUnknown)
  | Access(e, id) -> let l = find_variable_type env id in
                     let ind_expr = annotate_expr e env in
                     match l with
                     | Some(TList) ->
                        AAccess(ind_expr,
                                (annotate_expr id env),
                                TUnknown)
                     | _ -> failwith "Bad list access"


let rec annotate_stmt (s : Ast.stmt) (env : environment) : Sast.a_stmt =
  match s with
    Expr(e) -> AExpr(annotate_expr e env)
  | Block(e_list) -> ABlock(List.map (fun e -> annotate_expr e env) e_list)
  | If(e, s1, s2) -> let ae = annotate_expr e env in
                     if require_bool ae "Non-bool expression in if statement"
                     then AIf(ae,
                              annotate_stmt s1 env,
                              annotate_stmt s2 env)
                     else failwith "Impossible case in AIf"


let annotate_stage (s : Ast.stage) (env : environment) : Sast.a_stage =
  let new_env = { global_scope = env.global_scope;
                  local_scope = { variables = []; }; } in
  { sname = s.sname;
    body = List.map (fun stmt -> annotate_stmt stmt new_env) s.body;
    is_start = s.is_start; }


let annotate_recipe (r : Ast.recipe) : Sast.a_recipe =
  let new_env = { global_scope = {
                    variables = List.map (fun s -> (s, TUnknown)) r.formals;
                  };
                  local_scope = { variables = []; }; } in
  { rname = r.rname;
    formals = r.formals;
    body = List.map (fun stage -> annotate_stage stage new_env) r.body; }


let annotate_program (p : Ast.program) : Sast.a_program =
  let new_env = { global_scope = { variables = []; };
                  local_scope = { variables = []; }; } in
  { recipes = List.map annotate_recipe p.recipes;
    stages = List.map (fun stage -> annotate_stage stage new_env) p.stages; }

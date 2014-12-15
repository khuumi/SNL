open Ast
open Sast


module StringMap = Map.Make(String);;


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
  with Not_found -> match id with
                      Id(_, Global) -> Some(TUnknown)
                    | _ -> None


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
     ABinop(ae1, op, ae2, TUnknown)
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
                     AIf(ae,
                         annotate_stmt s1 env,
                         annotate_stmt s2 env)


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


let rec collect_outs (s : Sast.a_stage) : string list =
  List.fold_left collect_nexts_stmt [] s.body
and collect_nexts_stmt (l : string list) (s : Sast.a_stmt) : string list =
  match s with
    AExpr(ae) -> collect_nexts_expr l ae
  | ABlock(e_l) -> List.fold_left collect_nexts_expr l e_l
  | AIf(_, s1, s2) -> collect_nexts_stmt (collect_nexts_stmt l s1) s2
and collect_nexts_expr (l : string list) (e : Sast.a_expr) : string list =
  match e with
    ANext(s, _) -> if List.exists (fun name -> name = s) l
                   then l
                   else s :: l
  | _ -> l


let generate_stage_diagnostics (stages : Sast.a_stage list) :
      string list * string list =
  (* TODO: Make sure all stage names are unique. *)
  let snames = List.map (fun s -> s.sname) stages in
  let outs_graph = List.fold_left
                     (fun map stage -> StringMap.add
                                         stage.sname
                                         (collect_outs stage)
                                         map)
                     StringMap.empty
                     stages in
  let ins_graph = StringMap.fold
                    (fun name outs map ->
                     List.fold_left
                       (fun map out_name ->
                        if StringMap.mem out_name map
                        then StringMap.add
                               out_name
                               (name :: (StringMap.find out_name map))
                               map
                        else StringMap.add out_name [name] map)
                       map
                       outs)
                    outs_graph
                    StringMap.empty in
  let warnings = StringMap.fold
                   (fun name ins w -> if List.length ins = 0
                                      then ("Warning: stage " ^
                                              name ^
                                                " is not reachable.") :: w
                                      else w)
                   ins_graph
                   []
  and errors = StringMap.fold
                 (fun name outs e ->
                  List.fold_left
                    (fun e_l oname ->
                     if List.exists (fun n -> n = oname) snames
                     then e_l
                     else ("Error in stage " ^ name ^ ": next " ^
                             oname ^ " goes to an invalid stage.") :: e_l)
                    e
                    outs)
                 outs_graph
                 [] in
  warnings, errors

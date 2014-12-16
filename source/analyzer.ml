open Ast
open Sast


module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

  
let lib_funcs = [("show", 1); ("remove", 2); ("insert", 3);
                 ("append", 2); ("length", 1)];;


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
  | Block(s_list) -> ABlock(List.map (fun s -> annotate_stmt s env) s_list)
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
  | ABlock(s_l) -> List.fold_left collect_nexts_stmt l s_l
  | AIf(_, s1, s2) -> collect_nexts_stmt (collect_nexts_stmt l s1) s2
and collect_nexts_expr (l : string list) (e : Sast.a_expr) : string list =
  match e with
    ANext(s, _) -> if List.exists (fun name -> name = s) l
                   then l
                   else s :: l
  | _ -> l


(* Returns a set of the names of reachable stages and a list of errors with the
   names of invalid stages attempted to visit. *)
let rec visit_stages (queue : string list)
                     (visited : StringSet.t)
                     (stages)
                     (errors : string list) : StringSet.t * string list =
  if List.length queue = 0
  then visited, errors
  else let current = List.hd queue in
       let nexts = StringMap.find current stages in
       visit_stages
         ((List.tl queue) @
            (List.filter
               (fun name -> not(List.mem name queue) &&
                              not(StringSet.mem name visited) &&
                                StringMap.mem name stages)
               nexts))
         (StringSet.add current visited)
         stages
         (errors @
            List.map
              (fun inval -> "Error in stage " ^ current ^ ": next " ^
                              inval ^ " calls an invalid stage.")
              (List.filter
                 (fun name -> not(StringMap.mem name stages))
                 nexts))


(* Returns a list of warnings and a list of errors for unreachable and
   invalid stages. *)
let generate_stage_flow_diagnostics (stages : Sast.a_stage list) :
      string list * string list =
  let start = List.find (fun s -> s.is_start) stages in
  let visited, errors =
    visit_stages
      [start.sname]
      StringSet.empty
      (List.fold_left (fun map stage ->
                       StringMap.add stage.sname (collect_outs stage) map)
                      StringMap.empty
                      stages)
      []
  and stage_set = StringSet.of_list (List.map (fun s -> s.sname) stages) in
  let unreachable = StringSet.diff stage_set visited in
  (StringSet.fold
     (fun name warnings ->
      ("Warning: stage " ^ name ^ " is unreachable.") :: warnings)
     unreachable
     []), errors


(* Checks for duplicate strings in a list and returns the duplicates. *)
let dup_string_check (names : string list) : string list =
  StringMap.fold
    (fun name count dups ->
     if count > 1
     then name :: dups
     else dups)
    (List.fold_left
       (fun map name ->
        if StringMap.mem name map
        then StringMap.add name ((StringMap.find name map) + 1) map
        else StringMap.add name 1 map)
       StringMap.empty
       names)
    []


(* Returns a list of warnings and a list of errors.
   Warnings: if any stages are unreachable in the program.
   Errors: if multiple stages have the same name,
           if the number of stages marked start is not exactly one,
           if any stages try to call 'next' to a stage that was not defined.
*)
let generate_stage_diagnostics (stages : Sast.a_stage list) :
      string list * string list =
  let snames = List.map (fun s -> s.sname) stages in
  let dup_name_errors =
    List.map
      (fun name -> "Error: multiple stages named " ^ name ^ ".")
      (dup_string_check snames)
  and num_starts = List.length (List.filter (fun s -> s.is_start) stages) in
  let errors =
    if num_starts > 1
    then ["Error: more than one stage is marked start."] @ dup_name_errors
    else if num_starts < 1
    then ["Error: no stages marked start."] @ dup_name_errors
    else dup_name_errors in
  if List.length errors > 0
  then [], errors
  else generate_stage_flow_diagnostics stages


(* Check if multiple recipes have the same name. Returns a list of errors. *)
let generate_recipe_diagnostics (recipes : Sast.a_recipe list) =
  let rnames = List.map (fun r -> r.rname) recipes in
  List.map
    (fun name -> "Error: multiple recipes named " ^ name ^ ".")
    (dup_string_check rnames)


let rec collect_calls (s : Sast.a_stage) : (string * int) list =
  List.fold_left collect_calls_stmt [] s.body
and collect_calls_stmt (l : (string * int) list) (s : Sast.a_stmt) :
      (string * int) list =
  match s with
    AExpr(ae) -> collect_calls_expr l ae
  | ABlock(s_l) -> List.fold_left collect_calls_stmt l s_l
  | AIf(_, s1, s2) -> collect_calls_stmt (collect_calls_stmt l s1) s2
and collect_calls_expr (l : (string * int) list) (e : Sast.a_expr) :
      (string * int) list =
  match e with
    ACall(name, formals, _) -> (name, List.length formals) :: l
  | _ -> l


(* Check if all recipe calls are calls to library functions or user-defined
   functions. Also checks if the number of arguments is correct.
   Args:
     recipes: a list of recipes, assumed to have unique names
     stages: a list of stages
 *)
let generate_call_diagnostics (recipes : Sast.a_recipe list)
                              (stages : Sast.a_stage list) : string list =
  let rformals = List.fold_left
                   (fun l r -> (r.rname, List.length r.formals) :: l)
                   []
                   recipes @ lib_funcs in
  List.fold_left
    (fun list stage ->
     (List.fold_left
        (fun l name_formals ->
         let name = fst name_formals in
         let count = snd name_formals in
         if not(List.mem_assoc name rformals)
         then ("Error in stage " ^ stage.sname ^ ": call to " ^ name ^
                 " does not refer to a defined recipe.") :: l
         else let ecount = List.assoc name rformals in
              if ecount != count
              then ("Error in stage " ^ stage.sname ^ ": call to " ^ name ^
                      " expects " ^ (string_of_int ecount) ^ " arguments but " ^
                        (string_of_int count) ^ " provided.") :: l
              else l)
        []
        (collect_calls stage)) @ list)
    []
    stages


(* Returns a list of diagnostics (warnings and errors) and whether any of the
   diagnostics are fatal errors. *)
let generate_diagnostics (p : Sast.a_program) : string list * bool =
  let r_format name str = "In recipe " ^ name ^ ": " ^ str in
  let r_internal_call_errors =
    List.concat (List.map
                   (fun r -> List.map
                               (fun str -> r_format r.rname str)
                               (generate_call_diagnostics p.recipes r.body))
                   p.recipes)
  and r_internal_diagnostics, has_r_internal_errors =
    List.fold_left
      (fun pair r -> let r_internal_s_warnings, r_internal_s_errors =
                       generate_stage_diagnostics r.body in
                     ((fst pair) @
                        (List.map
                           (fun str -> r_format r.rname str)
                           r_internal_s_warnings) @
                          (List.map
                             (fun str -> r_format r.rname str)
                             r_internal_s_errors)),
                      snd pair || List.length r_internal_s_errors > 0)
      ([], false)
      p.recipes
  and r_errors = generate_recipe_diagnostics p.recipes
  and s_warnings, s_errors = generate_stage_diagnostics p.stages
  and c_errors = generate_call_diagnostics p.recipes p.stages in
  let all_diagnostics = (r_errors @ s_warnings @ s_errors @ c_errors @
                         r_internal_call_errors @ r_internal_diagnostics) in
  all_diagnostics, (has_r_internal_errors ||
                      List.length all_diagnostics - List.length s_warnings > 0)

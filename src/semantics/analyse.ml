open Syntax.Tree
open Syntax.Dict
open Printf

let propertyOffset = -4
let argumentOffset = 16
let variableIndex = ref 0

let create_def kind c = { d_kind=kind; d_type=c }

let get_class t env = let d = lookup t env in d.d_type

let find_method meth methods = 
  try (List.find (fun m -> meth == m.m_name.x_name) methods).m_name
  with Not_Found ->
    printf "Unknown Method: %s" meth; exit 1

let find_properties prop properties = 
  try List.find (fun n -> prop == n) (List.map (fun Prop(n, _) -> n) properties)
  with Not_Found ->
    printf "Unknown Property: %s" prop; exit 1

let anotate_classes classes env = 
  match classes with
  | c:cs -> printf "%s\n" c.c_name.x_name; 
            c.c_name.x_def <- create_def ClassDef c;
            let env' = define c.c_name.x_name c.c_name.x_def env in
              anotate_classes cs env'
  | _ -> env

let rec anotate_arguments args index env = 
  match args with
  | (Prop(x, t))::props -> x.x_def <- create_def VariableDef(argumentOffset + index) (get_class t env); 
                           let env' = define x.x_name x.x_def env in
                           anotate_arguments props (index+4) env'
  | _ -> env

let rec anotate_properties properties classname index =
  match properties with
  | (Prop(x, t))::props -> x.x_def <- create_def PropertyDef(propertyOffSet + index) (get_class t env); anotateProperties props classname (index-4)
  | _ -> ()

let rec annotate_expr expr env =
  match expr.e_guts with
  | Name n -> let d = lookup n.x_name env in n.x_def <- d.x_def; n.x_def
  | MethodCall (e, m, args) -> let t = annotate_expr e env in (** Voidtype **)
                               let c = t.d_type in
                                 m.x_def <- (find_method m.x_name c.c_methods).x_def; (** Handle static methods here? **)
                                 List.iter (fun x -> annotate_expr x env) args;
                                 m.x_def
  | Property (e, n) -> let t = annotate_expr e env in
                       let c = t.d_type in
                         n.x_def <- (find_properties n c.c_properties).x_def;
                         n.x_def

let rec anotate_stmt stmt env =
  match stmt with
  | Assign (e1, e2) -> annotate_expr e1; annotate_expr e2; env
  | Delc (n, t, e) -> annotate_expr e; n.x_def <- create_def VariableDef(variableIndex) t; variableIndex := !variableIndex - 4; define n.x_name n.x_def env
  | Call e -> annotate_expr e; env
  | Return r ->
    begin
      match r with
      | Some e -> anotate_expr e; env
      | None -> env
    end
  | Seq stmts -> 
    begin
      match stmts with
      | s:ss -> let env' = anotate_stmt s env in anotate_stmt Seq(ss) env'
      | [] -> env
    end

let rec anotate_methods methods index env =
  match methods with
  | m::ms -> m.m_name.x_def <- create_def MethodDef(index) (get_class m.m_type env);
             anotate_methods ms (index+4) env
  | _ -> ()

let anotate_body meth env =
  let newEnv = anotate_arguments meth.m_arg 0 env in
  variableIndex := 0;
  anotate_stmt meth.m_body newEnv

let anotate_bodies cls env =
  List.iter (fun m -> anotate_body m env) cls.c_methods

let anotate_members cls env =
  anotate_methods cls.c_methods env;
  anotate_properties cls.c_properties env;

let anotate_program Program(cs) =
  let env = anotate_classes cs empty in
  List.inter (fun c -> anotate_members c env) cs;
  List.inter (fun c -> anotate_bodies c env) cs;
  env
open Syntax.Tree
open Errors
open Lib.Int

let propertyOffset = 4
let argumentOffset = 16
let variableOffset = -4
let vtableOffset = 12
let variableIndex = ref 0

let create_def kind t = { d_kind=kind; d_type = t }

let create_me cls =
  Prop({x_name="me"; x_def=(create_def NoneKind VoidType)}, TempType cls.c_name.x_name)

let get_class t env = 
  match t with
  | TempType n -> let d = lookup n env in d.d_type
  | VoidType -> VoidType
  | ClassType c -> raise (DuplicateName c.c_name.x_name)

let find_method meth cls = 
  match cls with
  | ClassType c ->
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) c.c_methods).m_name with
          Not_found -> raise (UnknownName meth)
      end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName n)

let find_properties prop cls = 
  match cls with
  | ClassType c -> 
    begin
        try List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) c.c_properties) with 
          Not_found -> raise (UnknownName prop.x_name)
    end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName n)

let rec annotate_classes classes env = 
  match classes with
  | c::cs -> c.c_name.x_def <- create_def ClassDef (ClassType c);
            let env' = define c.c_name.x_name c.c_name.x_def env in
              annotate_classes cs env'
  | _ -> env

let rec annotate_arguments args index env = 
  match args with
  | (Prop(x, t))::props -> 
      x.x_def <- create_def (VariableDef(argumentOffset + index)) (get_class t env); 
      let env' = define x.x_name x.x_def env in
        annotate_arguments props (index+4) env'
  | _ -> env

let rec annotate_properties properties index env =
  match properties with
  | (Prop(x, t))::props -> 
      x.x_def <- create_def (PropertyDef(propertyOffset + index)) (get_class t env); 
      annotate_properties props (index+4) env
  | _ -> ()

let rec annotate_expr expr env =
  match expr.e_guts with
  | Name n -> let d = lookup n.x_name env in n.x_def <- d; n.x_def
  | Constant _ -> integer_def
  | MethodCall (e, m, args) -> 
      let t = annotate_expr e env in
      let c =  t.d_type in
      m.x_def <- (find_method m.x_name c).x_def;
      List.iter (fun x -> ignore(annotate_expr x env)) args;
      m.x_def  
  | Property (e, n) -> 
      let t = annotate_expr e env in
      let c =  t.d_type in
      n.x_def <- (find_properties n c).x_def;
      n.x_def
  | New n -> 
    let d = lookup n.x_name env in 
      begin
        match d.d_kind with
        | ClassDef -> n.x_def <- d; n.x_def
        | _ -> raise (InvalidNew n.x_name)
      end

let rec annotate_stmt stmt env =
  match stmt with
  | Assign (e1, e2) -> ignore(annotate_expr e1 env); ignore(annotate_expr e2 env); env
  | Delc (n, t, e) -> 
      ignore(annotate_expr e env); 
      n.x_def <- create_def (VariableDef(variableOffset - !variableIndex)) (get_class t env);
      variableIndex := !variableIndex + 4;
      define n.x_name n.x_def env
  | Call e -> ignore(annotate_expr e env); env
  | Return r ->
    begin
      match r with
      | Some e -> ignore(annotate_expr e env); env
      | None -> env
    end
  | Seq stmts -> 
    begin
      match stmts with
      | s::ss -> let env' = annotate_stmt s env in annotate_stmt (Seq(ss)) env'
      | [] -> env
    end

let rec annotate_methods methods index env =
  match methods with
  | m::ms -> 
      m.m_name.x_def <- create_def (MethodDef(vtableOffset + index, m.m_static)) (get_class m.m_type env);
      annotate_methods ms (index + 4) env
  | _ -> ()

let annotate_body meth cls env =
  if not meth.m_static then meth.m_arguments <- (create_me cls) :: meth.m_arguments;
  let newEnv = annotate_arguments meth.m_arguments 0 env in
  variableIndex := 0;
  ignore(annotate_stmt meth.m_body newEnv);
  meth.m_size <- !variableIndex

let annotate_bodies cls env =
  List.iter (fun m -> ignore(annotate_body m cls env)) cls.c_methods

let annotate_members cls env =
  annotate_methods cls.c_methods 0 env;
  annotate_properties cls.c_properties 0 env

let annotate_program (Program(cs)) =
  let env = annotate_classes cs (define "Int" integer_def empty) in
  List.iter (fun c -> annotate_members c env) cs;
  List.iter (fun c -> annotate_bodies c env) cs;
  env

open Syntax.Tree
open Kgen.Codegen
open Errors
open Lib.Lib_all
open Lib.Type
open Lib.String
open Lib.Object
open Printf

let propertyOffset = 4
let argumentOffset = 12
let variableOffset = -4
let vtableOffset = 12
let variableIndex = ref 0
let p_type = ref VoidType

let create_def kind t = { d_kind=kind; d_type = t }

let create_method i r = {m_name=i.m_name; m_type=r.m_type; m_static=i.m_static; m_size=i.m_size; m_arguments=r.m_arguments; m_body=r.m_body; m_main=false; m_replace=false; m_origin=Mine}

let create_origin i c =
  match i.m_origin with
  | Mine -> Inherited c.c_name.x_name
  | o -> o

let rec copy inherited cls=
  match inherited with
  | i::is -> {m_name=i.m_name; m_type=i.m_type; m_static=i.m_static; m_size=i.m_size; m_arguments=i.m_arguments; m_body=i.m_body; m_main=false; m_replace=false; m_origin = create_origin i cls} :: (copy is cls)
  | [] -> []

let create_me cls =
  Prop({x_name="Me"; x_def=(create_def NoneKind VoidType)}, TempType cls.c_name.x_name)

let get_class t env =
  match t with
  | TempType n -> let d = lookup n env in d.d_type
  | VoidType -> VoidType
  | _ -> t

let find_method meth cls =
  match cls with
  | ClassType c ->
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) c.c_methods).m_name.x_def with
          Not_found -> raise (UnknownName meth)
      end
  | ArrayClassType (c, _) ->
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) c.c_methods).m_name.x_def with
          Not_found -> raise (UnknownName meth)
      end
  | VoidType -> raise VoidOperation
  | NilType -> raise (UnknownName meth)
  | TempType n -> raise (UnannotatedName n)

let find_properties prop cls =
  match cls with
  | ClassType c ->
      begin
        try List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) c.c_properties) with
          Not_found -> raise (UnknownName prop.x_name)
      end
  | ArrayClassType (c, _) ->
      begin
        try List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) c.c_properties) with
          Not_found -> raise (UnknownName prop.x_name)
      end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName n)
  | NilType -> raise (UnknownName prop.x_name)

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
  match expr with
  | Name n -> let d = lookup n.x_name env in n.x_def <- d; n.x_def.d_type
  | Constant (_, d) -> get_class d env
  | String _ -> string_def.d_type;
  | TypeOf e -> ignore(annotate_expr e env); type_def.d_type
  | MethodCall (e, m, args) ->
      let c = annotate_expr e env in
      m.x_def <- find_method m.x_name c;
      List.iter (fun x -> ignore(annotate_expr x env)) args;
      m.x_def.d_type
  | Property (e, n) ->
      let c = annotate_expr e env in
      n.x_def <- (find_properties n c).x_def;
      n.x_def.d_type
  | Sub (e1, e2) ->
      ignore (annotate_expr e2 env);
      begin
        match annotate_expr e1 env with
        | ArrayClassType (_, d) -> d
        | _ -> raise InvalidSub
      end
  | New n ->
      let d = lookup n.x_name env in
        begin
          match d.d_kind with
          | ClassDef -> n.x_def <- d; n.x_def.d_type
          | _ -> raise (InvalidNew n.x_name)
        end
  | NewArray (n, e) ->
      let d = lookup n.x_name env in
        begin
          match d.d_type with
          | ArrayClassType(_, _) -> n.x_def <- d; ignore(annotate_expr e env); n.x_def.d_type
          | _ -> raise (InvalidNew n.x_name)
        end
  | Parent -> !p_type
  | Nil -> object_def.d_type

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
  | IfStmt (e, ts, fs) ->
      ignore(annotate_expr e env);
      ignore(annotate_stmt ts env);
      ignore(annotate_stmt fs env);
      env
  | WhileStmt (e, s) ->
      ignore(annotate_expr e env);
      ignore(annotate_stmt s env);
      env
  | ForStmt (init, step, test, body) ->
      let env' = annotate_stmt init env in
        ignore(annotate_stmt step env');
        ignore(annotate_expr test env');
        ignore(annotate_stmt body env');
      env
  | Seq stmts -> List.fold_left (fun env' s -> annotate_stmt s env') env stmts
  | Nop -> env

let rec annotate_methods methods index env =
  match methods with
  | m::ms ->
      m.m_name.x_def <- create_def (MethodDef(vtableOffset + index, m.m_static)) (get_class m.m_type env);
      annotate_methods ms (index + 4) env
  | _ -> ()

let annotate_body meth cls env =
  match meth.m_origin with
  | Mine ->
      if meth.m_main then mainMethod := cls.c_name.x_name ^ "." ^ meth.m_name.x_name;
      let newEnv = annotate_arguments meth.m_arguments 0 env in
      variableIndex := 0;
      p_type := ClassType cls;
      ignore(annotate_stmt meth.m_body newEnv);
      meth.m_size <- !variableIndex
  | _ -> ()

let annotate_bodies cls env =
  List.iter (fun m -> ignore(annotate_body m cls env)) cls.c_methods

let rec replace_method r inherited =
  match inherited with
  | i::is ->
      let same_method r i = r.m_name.x_name = i.m_name.x_name in
        if (same_method r i) then (create_method i r) :: is
        else i :: (replace_method r is)
  | [] -> []

let rec replace_methods inherited replacing cls =
  match replacing with
  | r::rs -> replace_method r (replace_methods inherited rs cls)
  | [] -> copy inherited cls

let rec split methods =
  match methods with
  | m::ms ->
      let (x,y) = split ms in
        if m.m_replace then (m::x, y)
        else (x, m::y)
  | [] -> ([], [])

let annotate_members cls env =
  annotate_methods cls.c_methods 0 env;
  annotate_properties cls.c_properties 0 env

let rec annotate_parent cls env =
  match cls.c_pname with
  | TempType _ ->
      let ClassType p = get_class cls.c_pname env in
        annotate_parent p env;
        cls.c_pname <- ClassType p;
        let (r, n) = split cls.c_methods in
          cls.c_methods <- (replace_methods p.c_methods r p) @ n;
          cls.c_properties <- p.c_properties @ cls.c_properties;
          cls.c_size <- 4 * (List.length cls.c_properties);
          cls.c_ancestors <- p :: p.c_ancestors;
  | _ -> ()

let modify_non_static c =
  let modify m =
    if not m.m_static then m.m_arguments <- (create_me c) :: m.m_arguments
    else ()
  in List.iter modify c.c_methods

let rec annotate_arrays clss env =
   match clss with
   | c::cs ->
      if c.c_array then
        begin
          c.c_name.x_def <- create_def ClassDef (ArrayClassType(c, (get_class c.c_pname env)));
          c.c_pname <- TempType "Array";
          let env' = define c.c_name.x_name c.c_name.x_def env in
            annotate_classes cs env'
        end
      else annotate_arrays cs env
   | [] -> env

let annotate_program (Program(cs)) =
  let env = annotate_classes cs start_env in
    let env' = annotate_arrays cs env in
      List.iter modify_non_static cs;
      List.iter (fun c -> annotate_parent c env') cs;
      List.iter (fun c -> annotate_members c env') cs;
      List.iter (fun c -> annotate_bodies c env') cs;
      env'

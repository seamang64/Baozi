open Syntax.Tree
open Kgen.Codegen
open Errors
open Lib.Lib_all
open Lib.Type
open Lib.String
open Lib.Object
open Lib.Array
open Printf

let propertyOffset = 4
let argumentOffset = 12
let variableOffset = -4
let vtableOffset = 12
let variableIndex = ref 0
let p_type = ref VoidType

let create_def kind t = { d_kind=kind; d_type = t }

(* Replace inheirited method with new method *)
let create_method i r =
  { m_name=i.m_name; m_type=r.m_type; m_static=i.m_static; m_size=i.m_size; m_arguments=r.m_arguments; m_body=r.m_body; m_main=false; m_replace=true; m_origin=Mine }

(* Get the origin of a method *)
let create_origin i c =
  match i.m_origin with
  | Mine -> Inherited c.c_name.x_name
  | o -> o

(* copy inheirited method *)
let copy i cls =
  { m_name=i.m_name; m_type=i.m_type; m_static=i.m_static; m_size=i.m_size; m_arguments=i.m_arguments; m_body=i.m_body; m_main=false; m_replace=false; m_origin = create_origin i cls }

let create_me cls =
  Prop({x_name="Me"; x_def=(create_def NoneKind VoidType)}, TempType (Ident cls.c_name.x_name))

let rec get_class d =
  match d with
  | ClassType c -> c
  | ArrayType _ -> array_class
  | GenericClassType (c, _) -> c
  | GenericType (_, d) -> get_class d
  | _ -> raise InvalidExpression

(* Get proper type from temp type *)
let rec get_temp_type t env =
  match t with
  | Ident n -> let d = lookup n env in d.d_type (* Find the name n in the enviroment *)
  | Array t' -> ArrayType (get_temp_type t' env) (* Recurse on the type of the array elements *)
  | Generic (n, ts) ->
      let c = get_class (lookup n env).d_type in (* Find the name in in the enviroment *)
        (* Then do the same for all the types that are passed to the type variables *)
        try GenericClassType (c, List.combine (List.map (fun g -> g.g_name.x_name) c.c_generics) (List.map (fun x -> get_temp_type x env) ts))
          with Invalid_argument _ -> raise InvalidGeneric

let get_type t env =
  match t with
  | TempType d -> get_temp_type d env
  | _ -> t

let rec print_temp_type t =
  match t with
  | Ident n -> n
  | Array d -> sprintf "Array of %s" (print_temp_type d)
  | _ -> "A Generic"

(* Find a method within an class *)
let find_method meth cls =
  match cls with
  | ClassType c ->
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) c.c_methods).m_name.x_def with
          Not_found -> raise (UnknownName meth)
      end
  | ArrayType _ ->
      (* Arrays only have methods from Array *)
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) array_class.c_methods).m_name.x_def with
          Not_found -> raise (UnknownName meth)
      end
  | GenericClassType (c, _) ->
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) c.c_methods).m_name.x_def with
          Not_found -> raise (UnknownName meth)
      end
  | GenericType (_, d) ->
      (* Generic types are treated as their most general type *)
      let c = get_class d in
      begin
        try (List.find (fun m -> meth = m.m_name.x_name) c.c_methods).m_name.x_def with
          Not_found -> raise (UnknownName meth)
      end
  | VoidType -> raise VoidOperation
  | NilType -> raise (UnknownName meth)
  | TempType n -> raise (UnannotatedName (print_temp_type n))

(* Find a property within an class *)
let find_properties prop cls =
  match cls with
  | ClassType c ->
      begin (* Find a method in c with the same name as meth *)
        try (List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) c.c_properties)).x_def with
          Not_found -> raise (UnknownName prop.x_name)
      end
  | ArrayType _ ->
      (* Arrays only have properties from Array *)
      begin
        try (List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) array_class.c_properties)).x_def with
          Not_found -> raise (UnknownName prop.x_name)
      end
  | GenericClassType (c, _) ->
      begin
        try (List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) c.c_properties)).x_def with
          Not_found -> raise (UnknownName prop.x_name)
      end
  | GenericType (_, d) ->
      (* Generic types are treated as their most general type *)
      let c = get_class d in
      begin
        try (List.find (fun n -> prop.x_name = n.x_name) (List.map (fun (Prop(n, _)) -> n) c.c_properties)).x_def with
          Not_found -> raise (UnknownName prop.x_name)
      end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName (print_temp_type n))
  | NilType -> raise (UnknownName prop.x_name)

(* Add classes to environment *)
let annotate_classes classes env =
  let annotate c e =
    c.c_name.x_def <- create_def ClassDef (ClassType c);
    define c.c_name.x_name c.c_name.x_def e
  in List.fold_right annotate classes env

(* Add Arguments to environment *)
let annotate_arguments args env =
  let rec annotate a i e =
    match a with
    | (Prop(x, t))::props ->
        x.x_def <- create_def (VariableDef(argumentOffset + i)) (get_type t e);
        let env' = define x.x_name x.x_def e in
          annotate props (i+4) env'
    | _ -> e
  in annotate args 0 env

let annotate_properties properties env =
  let rec annotate ps i e =
    match ps with
    | (Prop(x, t))::props ->
        x.x_def <- create_def (PropertyDef(propertyOffset + i)) (get_type t e);
        annotate props (i+4) e
    | _ -> ()
  in annotate properties 0 env

let rec annotate_expr expr env =
  match expr with
  | Name n -> let d = lookup n.x_name env in n.x_def <- d; n.x_def.d_type
  | Constant (_, d) -> get_type d env
  | String _ -> string_def.d_type;
  | TypeOf _ -> type_def.d_type
  | MethodCall (e, m, args) ->
      (* Annotate the calling object *)
      let c = annotate_expr e env in
      (* Find method that is being called *)
      m.x_def <- find_method m.x_name c;
      (* Annotate all the arguments *)
      List.iter (fun x -> ignore(annotate_expr x env)) args;
      (* Return the type of the method *)
      m.x_def.d_type
  | Property (e, n) ->
      (* Annotate the calling object *)
      let c = annotate_expr e env in
      (* Find the property in the calls *)
      n.x_def <- find_properties n c;
      (* Return the type of the property *)
      n.x_def.d_type
  | Sub (e1, e2) ->
      (* Annotate the index *)
      ignore (annotate_expr e2 env);
      begin
        match annotate_expr e1 env with
        | ArrayType d -> d        (* Cannot use sub notation on anything expect arrays *)
        | _ -> raise InvalidSub
      end
  | New n ->
      let d = get_type n.x_def.d_type env in
        n.x_def <- create_def ClassDef d; d
  | NewArray (n, e) ->
      ignore(annotate_expr e env);
      let d = get_type n.x_def.d_type env in
        n.x_def <- create_def ClassDef d; d
  | Parent -> !p_type
  | Nil -> object_def.d_type

let rec annotate_stmt stmt env =
  match stmt.s_guts with
  | Assign (e1, e2) -> ignore(annotate_expr e1 env); ignore(annotate_expr e2 env); env
  | Delc (n, t, e) ->
      (* Annotate the assinged value *)
      ignore(annotate_expr e env);
      (* Add this new variable to the environment *)
      n.x_def <- create_def (VariableDef(variableOffset - !variableIndex)) (get_type t env);
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

let annotate_generics env generic =
  match generic.g_ptype with
  | VoidType ->
      (* VoidType denotes no parent type. Most general type therefore is Object *)
      define generic.g_name.x_name (create_def ClassDef (GenericType (generic.g_name.x_name, (ClassType object_class)))) env
  | t ->
      let d = get_type t env in
      generic.g_ptype <- d;
      (* Create new generic type in enviroment with it's mpost general type *)
      define generic.g_name.x_name (create_def ClassDef (GenericType (generic.g_name.x_name, d))) env

let annotate_methods methods env=
  let rec annotate meths i e =
    match meths with
    | m::ms ->
        m.m_name.x_def <- create_def (MethodDef(vtableOffset + i, m.m_static)) (get_type m.m_type e);
        annotate ms (i + 4) e
    | _ -> ()
  in annotate methods 0 env

let annotate_body meth cls env =
  match meth.m_origin with
  | Mine -> (* Only annotate if this is a new method *)
      if meth.m_main then mainMethod := cls.c_name.x_name ^ "." ^ meth.m_name.x_name;
      let env' = List.fold_left annotate_generics env cls.c_generics in (* Add generic types to environment *)
      let newEnv = annotate_arguments meth.m_arguments env' in        (* Add arguments to environment *)
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

(* Split methods into replacing and normal *)
let rec split methods =
  match methods with
  | m::ms ->
      let (x,y) = split ms in
        if m.m_replace then (m::x, y)
        else (x, m::y)
  | [] -> ([], [])

let annotate_members cls env =
  let env' = List.fold_left annotate_generics env cls.c_generics in
    let generics =
      function
        | GenericClassType (_, ts) -> ts
        | _ -> [] in
      let env'' = List.fold_left (fun e (i, t) -> define i (create_def ClassDef t) e) env' (generics cls.c_ptype) in
        annotate_methods cls.c_methods env'';
        annotate_properties cls.c_properties env''

let rec annotate_parent cls env =
  match cls.c_ptype with
  | TempType _ -> (* If the parent has not been annotated *)
      let parent = get_type cls.c_ptype env in
        annotate_parent (get_class parent) env; (* Recursivly annotate the parent of the parent *)
        cls.c_ptype <- parent;                  (* Update own parent *)
        let (r, n) = split cls.c_methods and p = get_class parent in
          cls.c_methods <- (List.fold_right replace_method r (List.map (fun i -> copy i p) p.c_methods)) @ n;
          cls.c_properties <- p.c_properties @ cls.c_properties;
          cls.c_size <- 4 * (List.length cls.c_properties);
          cls.c_ancestors <- p :: p.c_ancestors
  | _ -> ()

let modify_non_static c =
  let modify m =
    if not m.m_static then m.m_arguments <- (create_me c) :: m.m_arguments
    else ()
  in List.iter modify c.c_methods

(* Annotate the whole program *)
let annotate_program (Program(cs)) =
  let env = annotate_classes cs start_env in
    List.iter modify_non_static cs;
    List.iter (fun c -> annotate_parent c env) cs;
    List.iter (fun c -> annotate_members c env) cs;
    List.iter (fun c -> annotate_bodies c env) cs
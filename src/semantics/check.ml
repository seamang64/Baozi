open Syntax.Tree
open Syntax.Dict
open Printf

let find_class t env =
  let d = try lookup t env with
    Not_found -> printf "Unkown type: %s" t; exit 1 in
  match d.x_def with
  | ClassDef -> d
  | _ -> expr2

let find_method method cls = 
  try List.find (fun m-> m.m_name == method) cls.c_methods with
  Not_found -> printf "Unkown method: %s" method

let find_propertyType prop cls = 
  try 
    let p = List.find (fun Prop(_, t) -> t == prop) cls.c_properties in 
      match p with
      | Prop(_, t) -> t
  with
  Not_found -> printf "Unkown Property: %s" prop

let rec check_args args margs c env =
  match (args, margs) with
  | (e::es, Prop(_, t)::ms) -> 
    if check_expr e c env != t 
    then printf "Error. Incorrect type. Expected %s" t; exit 1
    else check_args es ms c env
  | _ -> ()

let rec check_expr e types c env =
  match e.e_guts with
  | Name -> (** Find in env, else find in classes **)
  | MethodCall(e1, m, args) -> let t = check_expr t1 c env in
                               let m = find_method m (find_class t) in
                               check_args args m._arguments c env;
                               e.e_type <- m.m_type; m.m_type
  | Property(e1, n) -> let t = check_expr e1 c env in
                       let p = find_property_type n (find_class t) in
                       e.e_type <- p; p

let rec check_stmt s types ret c env =
  match s with
  | Assign(e1, e2) -> if check_expr e1 types c env != check_expr e2 types c env then
                      printf "Error in assigment"; exit 1
                      else ()
  | Delc(x, t, e) -> ignore(find_class t);
                     if check_expr e types c env != t then
                       printf "Error in delcaration"; exit 1
                     else ()
  | Call(e) -> match e with
               | MethodCall(e1, m, args) -> ignore (check_expr MethodCall(e1, m, args) c env)
               | _ -> "Incomplete statement"; exit 1
  | Return(r) -> match r with
                 | Some e -> 
                   let t = check_expr e types c env in
                   if ret != t then printf "Incorrect return type"; exit 1
                   else t
                 | None -> 
                   if ret != "voidtype"then printf "Incorrect return type"; exit 1
                   else "voidtype"
  | Seq(ss) -> List.iter (fun st -> check_stmt st types ret env) ss 

let check_property types prop =
  match prop with
  | Prop(_, t) -> ignore(find_class t)

let check_method c types meth = 
  List.iter (check_property types) meth.m_arguments;
  ignore(find_class meth.m_type);
  check_stmt meth.m_body types meth.m_type c.c_name.x_name(** Some env **)

let check_class types c =
  List.iter (check_property types) c.c_properties;
  List.iter (check_method c types) c.c_methods

let check_program program types = 
  match program with
  | Program(cs) -> List.iter (check_class types) cs
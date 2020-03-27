open Syntax.Tree
open Syntax.Dict
open Printf

let check_compatible t1 t2 =
  t1.c_name.x_name == t2.c_name.x_name

let find_method method cls = 
  try List.find (fun m-> m.m_name == method) cls.c_methods with
  Not_found -> printf "Unkown method: %s" method

let rec check_args args margs=
  match (args, margs) with
  | (e::es, Prop(x, _)::ms) -> 
    let t = check_expr e in
      if check_compatible t x.x_def.d_type
      then check_args es ms
      else printf "Incorrect type"; exit 1
  | _ -> ()

let rec check_expr e =
  match e.e_guts with
  | Name n -> n.x_def.d_type
  | MethodCall(e1, m, args) -> let t = check_expr e1 in
                               check_args args (find_method m t).m_arguments;
                               m.x_def.d_type
  | Property(e1, n) -> check_expr e1;
                       n.x_def.d_type

let rec check_stmt s ret =
  match s with
  | Assign(e1, e2) -> if check_compatible (check_expr e1) (check_expr e2) then
                      printf "Error in assigment"; exit 1
                      else ()
  | Delc(x, t, e) -> if check_compatible (x.x_def.d_type) (check_expr e2) then
                       printf "Error in delcaration"; exit 1
                     else ()
  | Call(e) -> match e with
               | MethodCall(e1, m, args) -> ignore (check_expr e)
               | _ -> "Incomplete statement"; exit 1
  | Return(r) -> match r with
                 | Some e -> 
                   let t = check_expr e in
                   if !(check_compatible ret t) then printf "Incorrect return type"; exit 1
                   else ()
                 | None -> 
                   if ret != "voidtype"then printf "Incorrect return type"; exit 1
                   else "voidtype"
  | Seq(ss) -> List.iter (fun st -> check_stmt st ret) ss 

let check_method meth = 
  check_stmt meth.m_body meth.m_name.x_def.d_type

let check_class c =
  List.iter check_method c.c_methods

let check_program program types = 
  match program with
  | Program(cs) -> List.iter (check_class types) cs
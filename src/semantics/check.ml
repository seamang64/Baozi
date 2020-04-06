open Syntax.Tree
open Printf

let check_compatible t1 t2 =
  match (t1, t2) with
  | (ClassType c1, ClassType c2) -> c1.c_name.x_name = c2.c_name.x_name
  | _ -> false

let find_method meth cls =
  match cls with
  | ClassType c ->
    begin
      try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
        Not_found -> printf "Unkown method: %s" meth.x_name; exit 1
    end
  | VoidType -> printf "Error in Method Call"; exit 1
  | _ -> printf "Error - Unassigned Class"; exit 1

let rec check_args args margs =
  match (args, margs) with
  | (e::es, Prop(x, _)::ms) -> 
      let t = check_expr e in
        if check_compatible t x.x_def.d_type then
          check_args es ms
        else (printf "Incorrect type"; exit 1)
  | _ -> ()

and check_expr e =
  match e.e_guts with 
  | Name n ->  n.x_def.d_type
  | MethodCall (e1, m, args) -> 
      let t = check_expr e1 in
        check_args args (find_method m t).m_arguments;
        m.x_def.d_type
  | Property (e1, n) -> 
      ignore(check_expr e1);
      n.x_def.d_type
  | New n -> n.x_def.d_type
  | _ -> printf "Unkown expression"; exit 1

let check_return r ret =
  match (r, ret) with
  | (Some e, ClassType _) -> 
    let t = check_expr e in
      if check_compatible t ret then ()
      else (printf "Incorrect Return statement"; exit 1)
  | (None, VoidType) -> ()
  | _ -> printf "Incorrect Return statement"; exit 1

let rec check_stmt s ret =
  match s with
  | Assign (e1, e2) -> (** Check e1 is property of variable, check e2 is not a class **)
      if check_compatible (check_expr e1) (check_expr e2) then ()
      else (printf "Error in assigment"; exit 1)
  | Delc (x, _, e) -> 
      if check_compatible x.x_def.d_type (check_expr e) then ()
      else (printf "Error in delcaration"; exit 1)
  | Call e ->
      begin
        match e.e_guts with
        | MethodCall(_, _, _) -> ignore (check_expr e)
        | _ -> printf "Incomplete statement"; exit 1
      end
  | Return r -> check_return r ret;
  | Seq(ss) -> List.iter (fun st -> check_stmt st ret) ss

let check_method meth = 
  check_stmt meth.m_body ( meth.m_name.x_def.d_type)

let check_class c =
  List.iter check_method c.c_methods

let check_program (Program(cs)) = 
  List.iter check_class cs
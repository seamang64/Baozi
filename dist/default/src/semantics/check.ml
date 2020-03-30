open Syntax.Tree
open Printf

let check_compatible t1 t2 =
  t1.c_name.x_name == t2.c_name.x_name

let find_method meth cls = 
  try List.find (fun m-> m.m_name == meth) cls.c_methods with
    Not_found -> printf "Unkown method: %s" meth.x_name; exit 1

let rec check_args args margs=
  match (args, margs) with
  | (e::es, Prop(x, _)::ms) -> 
      let t = check_expr e in
        if check_compatible t (Option.get x.x_def.d_type) then
          check_args es ms
        else printf "Incorrect type"; exit 1
  | _ -> ()

and check_expr e =
  match e.e_guts with 
  | Name n -> Option.get n.x_def.d_type
  | MethodCall (e1, m, args) -> 
      let t = check_expr e1 in
        check_args args (find_method m t).m_arguments;
        Option.get m.x_def.d_type
  | Property (e1, n) -> 
      ignore(check_expr e1);
      Option.get n.x_def.d_type
  | _ -> printf "Unkown expression"; exit 1

let rec check_stmt s ret =
  match s with
  | Assign (e1, e2) ->
      if check_compatible (check_expr e1) (check_expr e2) then ()
      else printf "Error in assigment"; exit 1
  | Delc (x, _, e) -> 
      if check_compatible (Option.get x.x_def.d_type) (check_expr e) then ()
      else printf "Error in delcaration"; exit 1
  | Call e ->
      begin
        match e.e_guts with
        | MethodCall(_, _, _) -> ignore (check_expr e)
        | _ -> printf "Incomplete statement"; exit 1
      end
  | Return r -> 
      begin
        match r with
        | Some e -> 
          let t = check_expr e in
            if check_compatible ret t then ()
            else printf "Incorrect return type"; exit 1
        | None -> 
          if ret.c_name.x_name == "voidtype" then ()
          else printf "Incorrect return type"; exit 1
      end
  | Seq(ss) -> List.iter (fun st -> check_stmt st ret) ss

let check_method meth = 
  check_stmt meth.m_body (Option.get meth.m_name.x_def.d_type)

let check_class c =
  List.iter check_method c.c_methods

let check_program (Program(cs)) = 
  List.iter check_class cs
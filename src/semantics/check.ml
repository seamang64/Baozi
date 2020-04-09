open Syntax.Tree
open Errors
open Lib.Int

let print_type t =
  match t with
  | ClassType c -> c.c_name.x_name
  | VoidType -> "Void"
  | TempType n -> n 

let check_compatible t1 t2 =
  match (t1, t2) with
  | (ClassType c1, ClassType c2) -> c1.c_name.x_name = c2.c_name.x_name
  | _ -> false

let find_method meth cls =
  match cls with
  | ClassType c ->
    begin
      try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
        Not_found -> raise (UnknownName meth.x_name)
    end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName n)

let rec check_args args margs =
  match (args, margs) with
  | (t::ts, Prop(x, _)::ms) -> 
      if check_compatible t x.x_def.d_type then
        check_args ts ms
      else raise (TypeError ((print_type t), (print_type x.x_def.d_type)))
  | ([], []) -> ()
  | _ -> raise IncorrectArgumentCount

and check_expr e =
  match e.e_guts with 
  | Name n ->  n.x_def.d_type
  | Constant _ -> integer_def.d_type
  | MethodCall (e1, m, args) -> 
      let t = check_expr e1 in
        let meth = find_method m t in
          if meth.m_static then check_args (List.map check_expr args) meth.m_arguments
          else  check_args (t::(List.map check_expr args)) meth.m_arguments;
          m.x_def.d_type
  | Property (e1, n) -> 
      ignore(check_expr e1);
      n.x_def.d_type
  | New n -> n.x_def.d_type
  | _ -> raise UnknownExpression

let check_return r ret =
  match (r, ret) with
  | (Some e, ClassType _) -> 
    let t = check_expr e in
      if check_compatible t ret then ()
      else raise (TypeError ((print_type t), (print_type ret)))
  | (None, VoidType) -> ()
  | _ -> raise InvalidReturn

let rec check_stmt s ret =
  match s with
  | Assign (e1, e2) -> (** Check e1 is property of variable, check e2 is not a class **)
      let (t1, t2) = ((check_expr e1), (check_expr e2)) in
        if check_compatible t1 t2 then ()
        else raise (TypeError ((print_type t1), (print_type t2)))
  | Delc (x, _, e) -> 
      let t = check_expr e in
        if check_compatible x.x_def.d_type t then ()
        else raise (TypeError (x.x_name, (print_type t)))
  | Call e ->
      begin
        match e.e_guts with
        | MethodCall(_, _, _) -> ignore (check_expr e)
        | _ -> raise IncompleteStatement
      end
  | Return r -> check_return r ret;
  | Seq(ss) -> List.iter (fun st -> check_stmt st ret) ss

let check_method meth = 
  check_stmt meth.m_body ( meth.m_name.x_def.d_type)

let check_class c =
  List.iter check_method c.c_methods

let check_program (Program(cs)) = 
  List.iter check_class cs
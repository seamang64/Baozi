open Syntax.Tree
open Errors
open Lib.Int
open Lib.Bool
open Lib.Type
open Lib.String
open Printf

let p_type = ref VoidType

let print_type t =
  match t with
  | ClassType c -> c.c_name.x_name
  | ArrayClassType (c, _) -> c.c_name.x_name
  | VoidType -> "Void"
  | TempType n -> n
  | NilType -> "Nil"

let rec check_compatible t1 t2 =
  match (t1, t2) with
  | (ClassType c1, ClassType c2) ->
      if c1.c_name.x_name == c2.c_name.x_name then ()
      else check_compatible t1 c2.c_pname
  | (ArrayClassType (c1, _), ArrayClassType (c2, _)) ->
      if c1.c_name.x_name != c2.c_name.x_name then raise (TypeError((print_type t1), (print_type t2)))
      else ()
  | (_, NilType) -> ()
  | _ -> raise (TypeError((print_type t1), (print_type t2)))

let find_method meth cls =
  match cls with
  | ClassType c ->
      begin
        try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | ArrayClassType (c, _) ->
      begin
        try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName n)
  | NilType -> raise (UnknownName meth.x_name)

let rec check_args args margs =
  match (args, margs) with
  | (t::ts, Prop(x, _)::ms) ->
      check_compatible x.x_def.d_type t;
      check_args ts ms
  | ([], []) -> ()
  | _ -> raise IncorrectArgumentCount

and check_expr e =
  match e with
  | Name n ->  n.x_def.d_type
  | Constant (_, d) ->
    begin
      match d with
      | TempType "Int" -> integer_def.d_type
      | TempType "Bool" -> bool_def.d_type
      | _ -> raise UnknownConstant
    end
  | String _ -> string_def.d_type
  | TypeOf e -> ignore(check_expr e); type_def.d_type
  | MethodCall (e1, m, args) ->
      let t = check_expr e1 in
        let meth = find_method m t in
          if meth.m_static then check_args (List.map check_expr args) meth.m_arguments
          else  check_args (t::(List.map check_expr args)) meth.m_arguments;
          m.x_def.d_type
  | Property (e1, n) ->
      ignore(check_expr e1);
      n.x_def.d_type
  | Sub (e1, e2) ->
      begin
        match check_expr e1 with
        | ArrayClassType (_, d1) ->
            let d2 = check_expr e2 in
              check_compatible d2 integer_def.d_type; d1
        | _ -> raise InvalidSub
      end
  | New n -> n.x_def.d_type
  | NewArray (n, e) ->
      let t = check_expr e in
        check_compatible t integer_def.d_type;
        n.x_def.d_type;
  | Parent -> !p_type
  | Nil -> NilType

let check_return r ret =
  match (r, ret) with
  | (Some e, ClassType _) ->
      let t = check_expr e in check_compatible t ret
  | (None, VoidType) -> ()
  | _ -> raise InvalidReturn

let rec check_stmt s ret =
  match s.s_guts with
  | Assign (e1, e2) ->
      let (t1, t2) = ((check_expr e1), (check_expr e2)) in
        check_compatible t1 t2
  | Delc (x, _, e) ->
      let t = check_expr e in
        check_compatible x.x_def.d_type t
  | Call e ->
      begin
        match e with
        | MethodCall(_, _, _) -> ignore (check_expr e)
        | _ -> raise IncompleteStatement
      end
  | Return r -> check_return r ret;
  | IfStmt (test, ts, fs) ->
      let t = check_expr test in
        check_compatible t bool_def.d_type;
        check_stmt ts ret;
        check_stmt fs ret
  | WhileStmt (test, stmt) ->
      let t = check_expr test in
        check_compatible t bool_def.d_type;
        check_stmt stmt ret
  | ForStmt (init, step, test, body) ->
      let t = check_expr test in
        check_stmt init ret;
        check_stmt step ret;
        check_compatible t bool_def.d_type;
        check_stmt body ret
  | Seq(ss) -> List.iter (fun st -> check_stmt st ret) ss
  | Nop -> ()

let check_method meth =
  check_stmt meth.m_body meth.m_name.x_def.d_type

let check_class c =
  p_type := ClassType c;
  List.iter check_method c.c_methods

let check_program (Program(cs)) =
  List.iter check_class cs
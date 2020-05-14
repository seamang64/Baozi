open Syntax.Tree
open Errors
open Lib.Int
open Lib.Bool
open Lib.Type
open Lib.String
open Lib.Array
open Printf

let p_type = ref VoidType

let rec print_temp_type t =
  match t with
  | Ident n -> n
  | Array t' -> sprintf "Array of %s" (print_temp_type t')

let rec print_type t =
  match t with
  | ClassType c -> c.c_name.x_name
  | ArrayType d -> sprintf "Array of %s" (print_type d)
  | GenericClassType (c, ts) -> (sprintf "%s with " c.c_name.x_name) ^ (List.fold_right (fun (n, t') s -> (print_type t') ^ s) ts "")
  | GenericType (n, d) -> sprintf "%s as %s" n (print_type d)
  | VoidType -> "Void"
  | TempType d -> sprintf "Temp %s" (print_temp_type d)
  | NilType -> "Nil"

let rec check__strict_compatible t1 t2 =
  match (t1, t2) with
  | (ClassType c1, ClassType c2) ->
      if c1.c_name.x_name == c2.c_name.x_name then ()
      else raise (TypeError((print_type t1), (print_type t2)))
  | (ArrayType d1, ArrayType d2) -> check_strict_compatible d1 d2
  | (GenericType (n1, d1), GenericType (n2, d2)) ->
      if n1 == n2 then ()
      else raise (TypeError((print_type t1), (print_type t2)))
  | _ -> check_compatible t1 t2;

and check_compatible t1 t2 =
  match (t1, t2) with
  | (ClassType c1, ClassType c2) ->
      if c1.c_name.x_name == c2.c_name.x_name then ()
      else check_compatible t1 c2.c_pname
  | (GenericClassType (c1, ts1), GenericClassType (c2, ts2)) ->
      check_compatible (ClassType c1) (ClassType c2);
      List.iter (fun ((_, x),(_, y)) -> check__strict_compatible x y) (List.combine ts1 ts2)
  | (ClassType c1, GenericClassType (c2,_)) ->
      if c1.c_name.x_name == c2.c_name.x_name then ()
      else check_compatible t1 c2.c_pname
  | (ArrayType d1, ArrayType d2) -> check_compatible d1 d2
  | (GenericType (n1, d1), GenericType (n2, d2)) ->
      if n1 == n2 then ()
      else check_compatible t1 d2
  | (_, NilType) -> ()
  | _ -> raise (TypeError((print_type t1), (print_type t2)))

let validate_generics generics =
  let rec validate prev =
    let validate_pair t gt =
      match gt with
      | GenericType (n1, d1) ->
          let (n, d) = List.find (fun (n2, d2) -> n1 == n2) prev in
            check_compatible d t
      | pt -> check_compatible pt t
      in function
        | (t, g)::gs ->
            validate_pair t g.g_ptype;
            validate ((g.g_name.x_name, g.g_ptype)::prev) gs
        | _ -> ()
  in validate [] generics


let rec validate_type t =
  match t with
  | GenericClassType (c, ts) ->
      let ds = List.map (fun (_,y) -> y) ts in
        List.iter validate_type ds; validate_generics (List.combine ds c.c_generics)
  | ArrayType t' -> validate_type t'
  | _ -> ()

let get_type ct t =
  let rec get_type_with_generic ts =
    function
    | GenericType (n, _) ->  let (_, d) = List.find (fun (i, _) -> i = n) ts in d
    | GenericClassType (c, ts') -> GenericClassType (c, List.map (fun (i,t') -> (i, get_type_with_generic ts t')) ts')
    | ArrayType d -> ArrayType (get_type_with_generic ts d)
    | t' -> t'
  in match (ct, t) with
  | (GenericClassType (_, ts), _) -> get_type_with_generic ts t
  | _ -> t

let rec get_class d =
  match d with
  | ClassType c -> c
  | ArrayType _ -> array_class
  | GenericClassType (c, _) -> c
  | GenericType (_, d) -> get_class d
  | _ -> raise InvalidExpression

let find_method meth cls =
  match cls with
  | ClassType c ->
      begin
        try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | ArrayType _ ->
      begin
        try List.find (fun m-> m.m_name.x_name = meth.x_name) array_class.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | GenericClassType (c, _) ->
      begin
        try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | GenericType (_, d) ->
      let c = get_class d in
      begin
        try List.find (fun m-> m.m_name.x_name = meth.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName (print_temp_type n))
  | NilType -> raise (UnknownName meth.x_name)

let rec check_args args margs =
  match (args, margs) with
  | (t::ts, m::ms) ->
      check_compatible m t;
      check_args ts ms
  | ([], []) -> ()
  | _ -> raise IncorrectArgumentCount

and check_expr e =
  match e with
  | Name n ->  n.x_def.d_type
  | Constant (_, d) ->
    begin
      match d with
      | TempType (Ident "Int") -> integer_def.d_type
      | TempType (Ident "Bool") -> bool_def.d_type
      | _ -> raise UnknownConstant
    end
  | String _ -> string_def.d_type
  | TypeOf e -> ignore(check_expr e); type_def.d_type
  | MethodCall (e1, m, args) ->
      let t = check_expr e1 in
        let meth = find_method m t in
          let margs = List.map (fun (Prop(x, _)) -> get_type t x.x_def.d_type) meth.m_arguments in
            if meth.m_static then check_args (List.map check_expr args) margs
            else  check_args (t::(List.map check_expr args)) margs;
            get_type t m.x_def.d_type
  | Property (e1, n) ->
      let t = check_expr e1 in
      get_type t n.x_def.d_type
  | Sub (e1, e2) ->
      begin
        match check_expr e1 with
        | ArrayType d1 ->
            let d2 = check_expr e2 in
              check_compatible d2 integer_def.d_type; d1
        | _ -> raise InvalidSub
      end
  | New n -> let d = n.x_def.d_type in validate_type d; d
  | NewArray (n, e) ->
      let t = check_expr e and d = n.x_def.d_type in
        validate_type d;
        check_compatible t integer_def.d_type; d
  | Parent -> !p_type
  | Nil -> NilType

let check_return r ret =
  match (r, ret) with
  | (Some e, ClassType _) ->
      let t = check_expr e in check_compatible t ret
  | (Some e, ArrayType _) ->
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
  p_type := c.c_pname;
  List.iter check_method c.c_methods

let check_program (Program(cs)) =
  List.iter check_class cs
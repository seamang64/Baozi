open Syntax.Tree
open Errors
open Lib.Int
open Lib.Bool
open Lib.Type
open Lib.String
open Lib.Array
open Printf

let p_type = ref VoidType (* Parent type *)

(* Get a string from Temporary type *)
let rec print_temp_type t =
  match t with
  | Ident n -> n
  | Array t' -> sprintf "Array of %s" (print_temp_type t')
  | Generic (n, ts) -> sprintf "%s With %s" n (List.fold_right (fun t s -> sprintf "%s, %s" (print_temp_type t) s) ts "")

(* Get a string from a type *)
let rec print_type t =
  match t with
  | ClassType c -> c.c_name.x_name
  | ArrayType d -> sprintf "Array of %s" (print_type d)
  | GenericClassType (c, ts) -> (sprintf "%s with " c.c_name.x_name) ^ (List.fold_right (fun (n, t') s -> (print_type t') ^ s) ts "")
  | GenericType (n, d) -> sprintf "%s as %s" n (print_type d)
  | VoidType -> "Void"
  | TempType d -> sprintf "Temp %s" (print_temp_type d)
  | NilType -> "Nil"

(* Check strict compatibility between types *)
let check_strict_comp type1 type2 =
  let pt1 = ref VoidType and pt2 = ref VoidType in
  let rec check t1 t2 =
    match (t1, t2) with
    | (ClassType c1, ClassType c2) ->
        if c1.c_name.x_name == c2.c_name.x_name then () (* Classes must be identical *)
        else raise (TypeError((print_type t1), (print_type t2)))
    | (ArrayType d1, ArrayType d2) -> check d1 d2 (* Recurse on the type of the array elements *)
    | (GenericType (n1, _), GenericType (n2, _)) ->
        if n1 == n2 then () (* Generic types must have the same name *)
        else raise (TypeError((print_type t1), (print_type t2)))
    | (_, NilType) -> () (* Nil is compatible with anything *)
    | _ -> raise (TypeError((print_type !pt1), (print_type !pt2)))
  in pt1 := type1; pt2 := type2; check type1 type2

(* Check to see if two types are compatible with each other *)
let check_compatible type1 type2 =
  let pt1 = ref VoidType and pt2 = ref VoidType in
  let rec check t1 t2 =
    match (t1, t2) with
    | (ClassType c1, ClassType c2) ->
        if c1.c_name.x_name == c2.c_name.x_name then ()
        else check t1 c2.c_ptype (* Recurse and check t1 against t2's paraent type *)
    | (GenericClassType (c1, ts1), GenericClassType (c2, ts2)) ->
        check (ClassType c1) (ClassType c2); (* Check the classes are compatible *)
        begin
          try List.iter2 (fun (_, x) (_, y) -> check_strict_comp x y) ts1 ts2 (* Then check that the type arguments are the same *)
          with Invalid_argument _ -> raise (TypeError((print_type !pt1), (print_type !pt2)))
        end
    | (ClassType c1, GenericClassType (c2,_)) ->
        if c1.c_name.x_name == c2.c_name.x_name then () (* Generic class can be compatible with normal class *)
        else check t1 c2.c_ptype
    | (ArrayType d1, ArrayType d2) -> check_strict_comp d1 d2  (* Arrays are invarient *)
    | (ClassType c, ArrayType _ ) ->
        if c.c_name.x_name == "Object" then () (* Arrays are subtypes of Object *)
        else raise (TypeError((print_type !pt1), (print_type !pt2)))
    | (GenericType (n1, d1), GenericType (n2, d2)) ->
        if n1 == n2 then () (* Generic types should have the same name or be in a subtype relation *)
        else check t1 d2
    | (ClassType c, GenericType(_, t)) -> check t1 t (* Check the most-general type of the generic against the class type *)
    | (_, NilType) -> () (* Nil is compatible with anything *)
    | (VoidType, VoidType) -> () (* Void is compatible with itself *)
    | _ -> raise (TypeError((print_type !pt1), (print_type !pt2)))
  in pt1 := type1; pt2 := type2; check type1 type2

(* Validate that the types supplied to a generic do not violate the restrictions *)
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

(* Make sure a type is valid *)
let rec validate_type t =
  match t with
  | GenericClassType (c, ts) ->
      let ds = List.map (fun (_,y) -> y) ts in
        List.iter validate_type ds; validate_generics (List.combine ds c.c_generics)
  | _ -> ()

(* Get the true type of something with a generic type *)
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

(* Get the class from a type *)
let rec get_class d =
  match d with
  | ClassType c -> c
  | ArrayType _ -> array_class
  | GenericClassType (c, _) -> c
  | GenericType (_, d) -> get_class d
  | _ -> raise InvalidExpression

(* Find a method in class *)
let find_method meth cls =
  match cls with
  | ClassType c ->
      begin (*Find a method in c with same name as meth *)
        try List.find (fun m -> meth.x_name = m.m_name.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | ArrayType _ ->
      (* Arrays only have methods from Array *)
      begin (*Find a method in Array with same name as meth *)
        try List.find (fun m -> meth.x_name = m.m_name.x_name) array_class.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | GenericClassType (c, _) ->
      begin
        try List.find (fun m -> meth.x_name = m.m_name.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | GenericType (_, d) ->
      (* Generic types are treated as their most general type *)
      let c = get_class d in
      begin (*Find a method in c with same name as meth *)
        try List.find (fun m -> meth.x_name = m.m_name.x_name) c.c_methods with
          Not_found -> raise (UnknownName meth.x_name)
      end
  | VoidType -> raise VoidOperation
  | TempType n -> raise (UnannotatedName (print_temp_type n))
  | NilType -> raise (UnknownName meth.x_name)

(* Type check the args supplied to a method call *)
let rec check_args args margs =
  match (args, margs) with
  | (t::ts, m::ms) ->
      check_compatible m t;
      check_args ts ms
  | ([], []) -> ()
  | _ -> raise IncorrectArgumentCount

(* Get the type of an expression *)
and check_expr e =
  match e with
  | Name n -> n.x_def.d_type
  | Constant (_, d) ->
    begin
      match d with
      | TempType (Ident "Int") -> integer_def.d_type
      | TempType (Ident "Bool") -> bool_def.d_type
      | _ -> raise UnknownConstant
    end
  | String _ -> string_def.d_type
  | TypeOf _ -> type_def.d_type
  | MethodCall (e1, m, args) ->
      let t = check_expr e1 in (* Get the type of the receiving object *)
        let meth = find_method m t in (* Find the method *)
          let margs = List.map (fun (Prop(x, _)) -> get_type t x.x_def.d_type) meth.m_arguments in (* check the arguments *)
            if meth.m_static then check_args (List.map check_expr args) margs
            else  check_args (t::(List.map check_expr args)) margs;
            get_type t m.x_def.d_type (* Get the type of the result *)
  | Property (e1, n) ->
      let t = check_expr e1 in (* Get the type of the bject *)
      get_type t n.x_def.d_type (* Get the type of the property *)
  | Sub (e1, e2) ->
      begin
        match check_expr e1 with (* Make sure we are calling Sub on an array *)
        | ArrayType d1 ->
            let d2 = check_expr e2 in
              check_compatible d2 integer_def.d_type; d1 (* Check that the index is an integer *)
        | _ -> raise InvalidSub
      end
  | New n -> let d = n.x_def.d_type in validate_type d; d (* Validate the type of the new object *)
  | NewArray (n, e) ->
      let t = check_expr e and d = n.x_def.d_type in
        validate_type d; (* Validate the type of the new object *)
        check_compatible t integer_def.d_type; d (* Check that the length is an integer *)
  | Cast (e, n) ->
      let t = check_expr e and d = n.x_def in
      begin
        match d.d_kind with
        | ClassDef -> check_compatible t d.d_type; d.d_type
        | _ -> raise InvalidExpression
      end
  | Parent -> !p_type
  | Nil -> NilType

(* Check that the method returns a value of the correct type *)
let check_return r ret =
  match (r, ret) with
  | (Some e, _) ->
      let t = check_expr e in check_compatible t ret
  | (None, VoidType) -> ()
  | _ -> raise InvalidReturn

(* Type-check a statement *)
let rec check_stmt s ret =
  match s.s_guts with
  | Assign (e1, e2) ->
      (* Check that the LHS of an assignment is compatible with the RHS *)
      let (t1, t2) = ((check_expr e1), (check_expr e2)) in
        check_compatible t1 t2
  | Delc (x, _, e) ->
      (* Check that the RHS of an declaration is compatible with the declared type*)
      let t = check_expr e in
        check_compatible x.x_def.d_type t
  | Call e ->
      begin
        match e with
        | MethodCall(_, _, _) -> ignore (check_expr e)
        | _ -> raise IncompleteStatement
      end
  | Return r -> check_return r ret; (* Check the returned value *)
  | IfStmt (test, ts, fs) ->
      let t = check_expr test in
        check_compatible t bool_def.d_type; (* Check that the conditon is a Boolean *)
        check_stmt ts ret;
        check_stmt fs ret
  | WhileStmt (test, stmt) ->
      let t = check_expr test in
        check_compatible t bool_def.d_type; (* Check that the conditon is a Boolean *)
        check_stmt stmt ret
  | ForStmt (init, step, test, body) ->
      let t = check_expr test in
        check_stmt init ret;
        check_stmt step ret;
        check_compatible t bool_def.d_type; (* Check that the conditon is a Boolean *)
        check_stmt body ret
  | Seq(ss) -> List.iter (fun st -> check_stmt st ret) ss
  | Nop -> ()

(* Type-check a method *)
let check_method meth parent =
  if meth.m_replace then (* If it replaces a method, make sure it is compatible with what it is replacing *)
  begin
    let p_meth = find_method meth.m_name parent and get_types args = List.map (fun (Prop(x, _)) -> get_type VoidType x.x_def.d_type) args in
      check_compatible p_meth.m_name.x_def.d_type meth.m_name.x_def.d_type;
      check_args (get_types (List.tl p_meth.m_arguments)) (get_types (List.tl meth.m_arguments));
  end;
  (* Check the body of the method *)
  check_stmt meth.m_body meth.m_name.x_def.d_type

(* Type-check a class *)
let check_class c =
  validate_type c.c_ptype;
  p_type := c.c_ptype;
  List.iter (fun m -> check_method m c.c_ptype) c.c_methods

(* Type check the whole program *)
let check_program (Program(cs)) =
  List.iter check_class cs
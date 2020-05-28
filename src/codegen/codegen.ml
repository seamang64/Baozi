open Syntax.Tree
open Syntax.Keiko
open Syntax.Lexer
open Errors
open Semantics
open Gc
open Printf

let p_name = ref "" (* Name of the parent class *)

(* Get the string indicating a type *)
let type_name t =
  match t with
  | ClassType c -> c.c_name.x_name
  | _ -> "Other"

(* Get the size of a class *)
let get_size x =
  match x.x_def.d_type with
  | ClassType c -> c.c_size
  | GenericClassType (c, _) -> c.c_size
  | _ -> raise (InvalidNew x.x_name)

(* Get a pointer to a class descriptor *)
let get_name n =
  match n.x_def.d_type with
    | ClassType c -> GLOBAL (c.c_name.x_name ^ ".%desc")
    | GenericClassType (c, _) -> GLOBAL (c.c_name.x_name ^ ".%desc")
    | _ -> raise (InvalidNew n.x_name)

(* Get the name of the parent class from the type *)
let get_pname t =
  match t with
    | ClassType c -> c.c_name.x_name
    | GenericClassType (c, _) -> c.c_name.x_name
    | _ -> raise IncorrectSyntaxError

(* Check if a method is static *)
let is_static m =
  match m.x_def.d_kind with
  | MethodDef(_, s) -> s
  | _ -> raise (UnknownName m.x_name)

(* Unbox a primitive *)
let unbox = SEQ [CONST 4; OFFSET; LOADW]

(* Generate code for getting the address assoicated with a name *)
let rec gen_name_addr n =
  match n.x_def.d_kind with
  | ClassDef -> SEQ [GLOBAL (n.x_name ^ ".%desc")]
  | VariableDef off -> SEQ [LOCAL off]
  | PropertyDef off -> SEQ [CONST off; OFFSET]
  | MethodDef (off, _) -> SEQ [CONST off; OFFSET]
  | NoneKind -> raise (NoneKindError n.x_name)

(* Generate code for an address *)
and gen_addr expr =
  match expr with
  | Name n -> gen_name_addr n
  | Property (e, n) -> SEQ[gen_expr e; gen_name_addr n]
  | Sub (e1, e2) ->
      SEQ [
        (* Get the array the we are indexing from *)
        gen_expr e1;
        (* Get the "Data" *)
        CONST 4;
        OFFSET;
        LOADW;
        (* Get the index *)
        gen_expr e2;
        unbox;
        (* The element we want is at offset 4*i *)
        CONST 4;
        BINOP Times;
        OFFSET;
      ]
    | _ -> raise InvalidAssigment

(* Generate code for an expression *)
and gen_expr e =
  match e with
  | Name n -> SEQ [gen_name_addr n; LOADW]
  | Constant (x, d) ->
    begin
      match d with
      | TempType (Ident "Int") -> SEQ [CONST x; GLOBAL "baozi.%makeInt"; CALLW 1]
      | TempType (Ident "Bool") ->
          if x = 0 then GLOBAL "baozi.%const.%false"
          else GLOBAL "baozi.%const.%true"
      | _ -> raise UnknownConstant
    end
  | String (lab, s) -> (* A String constant *)
       SEQ [
        GLOBAL lab;
        CONST (String.length s);
        GLOBAL "baozi.%makeString";
        CALLW 2
      ]
  | TypeOf n -> (* A TypeOf expression creates a Type object *)
      SEQ [
        GLOBAL (n.x_name ^ ".%desc");
        GLOBAL "Type.%desc";
        GLOBAL "baozi.%makePrim";
        CALLW 2;
      ]
  | MethodCall (e1, m, args) ->
      if is_static m then gen_static_call e1 m args
      else gen_call e1 m args
  | Property (_, _) -> SEQ [gen_addr e; LOADW]
  | Sub (_, _) -> SEQ [gen_addr e; LOADW]
  | New n ->
      SEQ [
        (* get the address of the class descriptor *)
        get_name n;
        (* need space for all the properties + class descriptor *)
        CONST (4 + (get_size n));
        (* Call the make proceduure *)
        GLOBAL "baozi.%make";
        CALLW 2;
      ]
  | NewArray (_, e) ->
      SEQ [
        gen_expr e;
        GLOBAL "baozi.%makeArray";
        CALLW 1;
      ]
  | Cast (e, n) ->
      SEQ [
        gen_expr e;
        DUP 0;
        GLOBAL (n.x_name ^ ".%desc");
        GLOBAL "baozi.%typeCheck";
        CALL 2;
      ]
  | Parent -> SEQ [LOCAL 12; LOADW]
  | Nil -> LDG "Nil"

(* Generate code for a static call *)
and gen_static_call expr meth args =
  match expr with
  | Name n ->
      SEQ [
        (* evaluate that arguments *)
        SEQ (List.map gen_expr (List.rev args));
        (* get the address of the method *)
        GLOBAL (n.x_name ^"." ^ meth.x_name);
        (* Call the method *)
        CALLW (List.length args)
      ]
  | _ -> raise IncorrectSyntaxError

(* Generate the code for a non-static method call *)
and gen_call expr meth args =
  match expr with
  | Parent -> (* If the method is called on Parent, use the parent's method *)
      SEQ [
        (* evaluate the arugments *)
        SEQ (List.map gen_expr (List.rev args));
        (* evaluate the calling object as an argument *)
        gen_expr expr;
        (* get the method from the parent *)
        GLOBAL (!p_name ^ "." ^ meth.x_name);
        (* call the method *)
        CALLW (List.length args + 1)
      ]
  | _ ->
      SEQ [
        (* evaluate the arugments *)
        SEQ (List.map gen_expr (List.rev args));
        (* evaluate the calling object *)
        gen_expr expr;
        (* duplicate, as the calling object is also and argument *)
        DUP 0;
        (* Load the class descriptor from the object *)
        LOADW;
        (* find the offset for the method *)
        gen_name_addr meth;
        (* load the method *)
        LOADW;
        (* call the method *)
        TYPE (type_name (Check.check_expr expr));
        CALLW (List.length args + 1)
      ]

(* Gererate code for a conditional jump *)
and gen_cond tlab flab test =
  SEQ [
    gen_stack_maps (gen_expr test);
    unbox;
    CONST 0;
    JUMPC (Neq, tlab);
    JUMP flab
  ]

(*Generate code for a statement *)
and gen_stmt stmt =
  let rec code s =
    match s.s_guts with
    | Assign (e1, e2) -> SEQ [gen_stack_maps (SEQ [gen_expr e2; gen_addr e1]); STOREW]
    | Delc (n, _, e) -> SEQ [gen_stack_maps (gen_expr e); gen_name_addr n; STOREW]
    | Call e  -> gen_stack_maps (gen_expr e)
    | Return r ->
      begin
        match r with
        | Some e -> SEQ [gen_stack_maps (gen_expr e); RETURN 1]
        | None -> SEQ [RETURN 0]
      end
    | IfStmt (e, ts, fs) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [
          gen_cond lab1 lab2 e; (* Generate code for the condition *)
          LABEL lab1;
          gen_stmt ts; (* Generate code for the body *)
          JUMP lab3;
          LABEL lab2;
          gen_stmt fs; (* Generate code for the else body *)
          LABEL lab3;
        ]
    | WhileStmt (test, stmt) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [
          JUMP lab2;
          LABEL lab1;
          gen_stmt stmt; (* Generate code for  the body *)
          LABEL lab2;
          gen_cond lab1 lab3 test; (* Generate code for the condition *)
          LABEL lab3
        ]
    | ForStmt (init, step, test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
          SEQ [
            code init; (* Generate code for the initial statement *)
            JUMP lab2;
            LABEL lab1;
            gen_stmt body; (* Generate code for the body *)
            gen_stmt step; (* Generate code for the step *)
            LABEL lab2;
            gen_cond lab1 lab3 test;
            LABEL lab3
          ]
    | Seq ss -> SEQ (List.map gen_stmt ss)
    | Nop -> NOP
  in SEQ [ LINE stmt.s_line; code stmt ]

(* Generate code for a method *)
and gen_method classname m =
  match m.m_origin with
  | Mine -> (* If it is a new method, generate the code *)
      SEQ [
        PROC (classname ^ "." ^ m.m_name.x_name, m.m_size, 0, gen_proc_gc_map m.m_size (List.length m.m_arguments));
        Peepopt.optimise (gen_stmt m.m_body); (* Optimise the code *)
        END
      ]
  | Inherited _ -> NOP

(* Generate code for all methods in a class *)
and gen_methods c = p_name := get_pname c.c_ptype; SEQ (List.map (gen_method c.c_name.x_name) c.c_methods)

(* Create a pointer to the correct procedure *)
and gen_method_name meth cls =
  match meth.m_origin with
  | Mine -> WORD (SYMBOL (cls.c_name.x_name ^ "." ^ meth.m_name.x_name))
  | Inherited n -> WORD (SYMBOL (n ^ "." ^ meth.m_name.x_name))

(* Generate a hex string from a char *)
and gen_hex_string c =
  let chr = Char.code c and hex = "0123456789ABCDEF" in
    (Char.escaped (hex.[chr / 16])) ^ (Char.escaped (hex.[chr mod 16]))

(* Create a hex string from a string *)
and fold_string s =
  match s with
  | "" -> ""
  | _ -> (gen_hex_string (s.[0])) ^ (fold_string (String.sub s 1 ((String.length s) - 1)))

(* Generate a definintion in the data segment for a string constant *)
and gen_string (lab, s) =
  let strings = split_string s in
    let string_code = List.map (fun s -> STRING (fold_string s)) strings in
      SEQ [COMMENT (sprintf "String \"%s\"" s); DEFINE lab; SEQ string_code]

(* Split the a string into 32 bit chunks *)
and split_string s =
  let n = String.length s in
    if n > 31 then
      (String.sub s 0 32) :: (split_string (String.sub s 32 (n-32)))
    else [s ^ "\000"]

(* Generate code for a class *)
and gen_class c =
  let name = c.c_name.x_name in
  SEQ [
    COMMENT (sprintf "Descriptor for %s" name);
    DEFINE (name ^ ".%desc");
    WORD (SYMBOL (gen_class_gc_map c.c_size)); (* Create GC Map *)
    WORD (SYMBOL (name ^ ".%anc"));
    WORD (SYMBOL (name ^ ".%string"));
    SEQ (List.map (fun m -> gen_method_name m c) c.c_methods);
    COMMENT (sprintf "Ancestor Table for %s" name);
    DEFINE (name ^ ".%anc"); (* Create ancestor table *)
    WORD (DEC (1 + (List.length c.c_ancestors)));
    WORD (SYMBOL (name ^ ".%desc"));
    SEQ (List.map (fun c -> WORD (SYMBOL (c.c_name.x_name ^ ".%desc"))) c.c_ancestors);
    gen_string (name ^ ".%string", name);
  ]

(* Generate code for the whole program *)
and gen_program (Program(cs)) =
  SEQ [
    SEQ (List.map gen_methods cs);
    SEQ (List.map gen_class cs);
    SEQ (List.map gen_string !strtable);
    (* Create the MAIN method *)
    PROC ("MAIN", 0, 0, 0);
    GLOBAL !Analyse.mainMethod;
    CALLW 0;
    RETURN 0;
    END;
  ]

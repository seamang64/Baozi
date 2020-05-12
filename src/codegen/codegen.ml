open Syntax.Tree
open Syntax.Keiko
open Syntax.Lexer
open Errors
open Gc
open Printf
open Lib.Lib_all

let me_pointer = 4
let mainMethod = ref ""

let get_size x =
  match x.x_def.d_type with
  | ClassType c -> c.c_size
  | _ -> raise (InvalidNew x.x_name)

let is_int x =
  match x.x_def.d_type with
  | ClassType c -> c.c_name.x_name = "Integer"
  | _ -> false

let is_static m =
  match m.x_def.d_kind with
  | MethodDef(_, s) -> s
  | _ -> raise (UnknownName m.x_name)

let unbox = SEQ [CONST 4; OFFSET; LOADW]

let rec gen_primitive x desc =
  SEQ [
    CONST x;
    GLOBAL desc;
    GLOBAL "baozi.makePrim";
    CALLW 2
  ]

and gen_addr n =
  match n.x_def.d_kind with
  | ClassDef -> SEQ [GLOBAL (n.x_name ^ ".%desc")]
  | VariableDef off -> SEQ [LOCAL off]
  | PropertyDef off -> SEQ [CONST off; OFFSET]
  | MethodDef (off, _) -> SEQ [CONST off; OFFSET]
  | NoneKind -> raise (NoneKindError n.x_name)

and gen_expr e =
  match e with
  | Name n -> SEQ [gen_addr n; LOADW]
  | Constant (x, d) ->
    begin
      match d with
      | TempType "Int" -> gen_primitive x "Integer.%desc"
      | TempType "Bool" -> gen_primitive x "Bool.%desc"
      | _ -> raise UnknownConstant
    end
  | String (lab, s) ->
       SEQ [
        GLOBAL lab;
        CONST (String.length s);
        GLOBAL "baozi.makeString";
        CALLW 2
      ]
  | TypeOf (Name n) ->
      SEQ [
        GLOBAL (n.x_name ^ ".%desc");
        GLOBAL "Type.%desc";
        GLOBAL "baozi.makePrim";
        CALLW 2;
      ]
  | MethodCall (e1, m, args) ->
      if is_static m then gen_static_call e1 m args
      else gen_call e1 m args
  | Property (e1, n) -> SEQ [gen_expr e1; gen_addr n; LOADW]
  | Sub (e1, e2) ->
      SEQ [
        (* Get the array the we are indexing from *)
        gen_expr e1;
        (* Get the "Data" *)
        CONST 4;
        OFFSET;
        LOADW;
        CONST 4;
        OFFSET;
        (* Get the index *)
        gen_expr e2;
        unbox;
        (* The element we want is at offset 4*i *)
        CONST 4;
        BINOP Times;
        OFFSET;
        (* Load the element *)
        LOADW;
      ]
  | New n ->
      SEQ [
        (* get the address of the class descriptor *)
        GLOBAL (n.x_name ^ ".%desc");
        (* need space for all the properties + class descriptor *)
        CONST (4 + (get_size n));
        (* Call the make proceduure *)
        GLOBAL "baozi.make";
        CALLW 2;
      ]
  | NewArray (n, e) ->
      SEQ [
        GLOBAL (n.x_name ^ ".%desc");
        gen_expr e;
        GLOBAL "baozi.makeArray";
        CALLW 2;
      ]
  | Parent -> SEQ [LOCAL 12; LOADW]
  | Nil -> LDG "Nil"
  | _ -> raise UnknownExpression

and gen_static_call (Name n) meth args =
  SEQ [
    (* evaluate that arguments *)
    SEQ (List.map gen_expr (List.rev args));
    (* get the address of the method *)
    GLOBAL (n.x_name ^"." ^ meth.x_name);
    (* Call the method *)
    CALLW (List.length args)
  ]

and gem_static_call _ _ _ = raise IncorrectSyntaxError

and gen_call expr meth args =
  match expr with
  | Parent ->
      SEQ [
        (* evaulate arguments *)
        SEQ (List.map gen_expr (List.rev args));
        (* evaulate calling object *)
        gen_expr expr;
        (* duplicate, as the calling object is also an argument *)
        DUP 0;
        (* Get the object's class descriptor *)
        LOADW;
        (* Get the ancestor table at *)
        CONST 4;
        OFFSET;
        LOADW;
        (* Get the parent class descriptor *)
        CONST 8;
        OFFSET;
        LOADW;
        (* get the offset for the method *)
        gen_addr meth;
        (* load the method *)
        LOADW;
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
        gen_addr meth;
        (* load the method *)
        LOADW;
        (* call the method *)
        CALLW (List.length args + 1)
      ]

and gen_cond tlab flab test =
  SEQ [
    gen_stack_maps (gen_expr test);
    unbox;
    CONST 0;
    JUMPC (Neq, tlab);
    JUMP flab
  ]

and gen_assigment e1 e2 =
  let code =
    let v = gen_expr e2 in
    match e1 with
    | Name n -> SEQ [v; gen_addr n; STOREW]
    | Property (e, n) -> SEQ[v; gen_expr e; gen_addr n; STOREW]
    | Sub (e3, e4) ->
        SEQ [
          v;
          gen_expr e3;
          CONST 4;
          OFFSET;
          LOADW;
          CONST 4;
          OFFSET;
          gen_expr e4;
          unbox;
          CONST 4;
          BINOP Times;
          OFFSET;
          STOREW;
        ]
    | _ -> raise InvalidAssigment
  in gen_stack_maps code

and gen_stmt stmt =
  let code s =
    match s.s_guts with
    | Assign (e1, e2) -> gen_assigment e1 e2
    | Delc (n, _, e) -> SEQ [gen_stack_maps (gen_expr e); gen_addr n; STOREW]
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
          gen_cond lab1 lab2 e;
          LABEL lab1;
          gen_stmt ts;
          JUMP lab3;
          LABEL lab2;
          gen_stmt fs;
          LABEL lab3;
        ]
    | WhileStmt (test, stmt) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [
          JUMP lab2;
          LABEL lab1;
          gen_stmt stmt;
          LABEL lab2;
          gen_cond lab1 lab3 test;
          LABEL lab3
        ]
    | ForStmt (init, step, test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
          SEQ [
            gen_stmt init;
            JUMP lab2;
            LABEL lab1;
            gen_stmt body;
            gen_stmt step;
            LABEL lab2;
            gen_cond lab1 lab3 test;
            LABEL lab3
          ]
    | Seq ss -> SEQ (List.map gen_stmt ss)
    | Nop -> NOP
  in SEQ [ LINE stmt.s_line; code stmt ]

and gen_method classname m =
  match m.m_origin with
  | Mine ->
      SEQ [
        PROC (classname ^ "." ^ m.m_name.x_name, m.m_size, 0, gen_proc_gc_map m.m_size (List.length m.m_arguments));
        Peepopt.optimise (gen_stmt m.m_body);
        END
      ]
  | Inherited _ -> NOP

and gen_methods c = SEQ (List.map (gen_method c.c_name.x_name) c.c_methods)

and gen_method_name meth cls =
  match meth.m_origin with
  | Mine -> WORD (SYMBOL (cls.c_name.x_name ^ "." ^ meth.m_name.x_name))
  | Inherited n -> WORD (SYMBOL (n ^ "." ^ meth.m_name.x_name))

and gen_hex_string c =
  let chr = Char.code c and hex = "0123456789ABCDEF" in
    (Char.escaped (hex.[chr / 16])) ^ (Char.escaped (hex.[chr mod 16]))

and fold_string s =
  match s with
  | "" -> ""
  | _ -> (gen_hex_string (s.[0])) ^ (fold_string (String.sub s 1 ((String.length s) - 1)))

and gen_string (lab, s) =
  let strings = split_string s in
    let string_code = List.map (fun s -> STRING (fold_string s)) strings in
      SEQ [COMMENT (sprintf "String \"%s\"" s); DEFINE lab; SEQ string_code]

and split_string s =
  let n = String.length s in
    if n > 31 then
      (String.sub s 0 32) :: (split_string (String.sub s 32 (n-32)))
    else [s ^ "\000"]

and gen_nil =
  SEQ [
    GLOBAL "Object.%desc";
    CONST 4;
    GLOBAL "baozi.make";
    CALLW 2;
    STG "Nil";
  ]

and gen_class c =
  let name = c.c_name.x_name in
  SEQ [
    COMMENT (sprintf "Descriptor for %s" name);
    DEFINE (name ^ ".%desc");
    WORD (SYMBOL (gen_class_gc_map c.c_size));
    WORD (SYMBOL (name ^ ".%anc"));
    WORD (SYMBOL (name ^ ".%string"));
    SEQ (List.map (fun m -> gen_method_name m c) c.c_methods);
    COMMENT (sprintf "Ancestor Table for %s" name);
    DEFINE (name ^ ".%anc");
    WORD (DEC (1 + (List.length c.c_ancestors)));
    WORD (SYMBOL (name ^ ".%desc"));
    SEQ (List.map (fun c -> WORD (SYMBOL (c.c_name.x_name ^ ".%desc"))) c.c_ancestors);
    gen_string (name ^ ".%string", name);
  ]

and gen_program (Program(cs)) =
  SEQ [
    (* SEQ lib_define_code;
    SEQ (List.map (fun k -> SEQ (List.map Peepopt.optimise k)) lib_method_code); *)
    SEQ (List.map gen_methods cs);
    SEQ (List.map gen_class cs);
    SEQ (List.map gen_string !strtable);
    PROC ("MAIN", 0, 0, 0);
    (* CONST 0;
    GLOBAL "lib.start_clock";
    PCALLW 0; *)
    GLOBAL !mainMethod;
    CALLW 0;
    (* CONST 0;
    GLOBAL "lib.end_clock";
    PCALLW 0; *)
    RETURN 0;
    END;
  ]

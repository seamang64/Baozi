open Syntax.Tree
open Syntax.Keiko
open Errors
open Printf
open Lib.Int
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

let gen_primitive x desc =
  SEQ [
    CONST 8;
    GLOBAL desc;
    GLOBAL "lib.new";
    CALLW 2;
    DUP 0;
    CONST x;
    SWAP;
    CONST 4;
    OFFSET;
    STOREW;
    DUP 0;
    GLOBAL desc;
    SWAP;
    STOREW;
  ]

let gen_addr n =
  match n.x_def.d_kind with
  | ClassDef -> SEQ [GLOBAL (n.x_name ^ ".%desc")]
  | VariableDef off -> SEQ [LOCAL off]
  | PropertyDef off -> SEQ [CONST off; OFFSET]
  | MethodDef (off, _) -> SEQ [CONST off; OFFSET]

let rec gen_expr e = 
  match e.e_guts with
  | Name n ->
    begin
      match n.x_def.d_kind with
      | ClassDef -> SEQ [GLOBAL (n.x_name ^ ".%desc")]
      | _ -> SEQ [gen_addr n; LOADW]
    end
  | Constant (x, d) ->
    begin
      match d with
      | TempType "Int" -> gen_primitive x "Integer.%desc"
      | TempType "Bool" -> gen_primitive x "Bool.%desc"
      | _ -> raise UnknownConstant
    end
  | MethodCall (e1, m, args) ->
      SEQ [ 
        SEQ (List.map gen_expr (List.rev args));
        gen_expr e1;
        if is_static m then
          SEQ [ 
            gen_addr m;
            LOADW;
            CALLW (List.length args)
          ]
        else 
          begin
            match e1.e_guts with
            | Parent -> 
                SEQ [
                  DUP 0;
                  LOADW;
                  CONST 4;
                  OFFSET;
                  LOADW;
                  LOADW;
                  gen_addr m;
                  LOADW;
                  CALLW (List.length args + 1)
                ]
            | _ -> 
                SEQ [ 
                  DUP 0; 
                  LOADW;
                  gen_addr m;
                  LOADW;
                  CALLW (List.length args + 1)
                ]
          end
      ]
  | Property (e1, n) -> SEQ [gen_expr e1; gen_addr n; LOADW]
  | Sub (e1, e2) -> 
      SEQ [
        gen_expr e1;
        gen_expr e2;
        CONST 4;
        OFFSET;
        LOADW;
        CONST 1;
        BINOP Plus;
        CONST 4;
        BINOP Times;
        OFFSET;
        LOADW;
      ]
  | New n ->  
      SEQ [
        CONST (4 + (get_size n));
        GLOBAL (n.x_name ^ ".%desc"); 
        GLOBAL "lib.new"; 
        CALLW 2;
        DUP 0;
        GLOBAL (n.x_name ^ ".%desc");
        SWAP;
        STOREW;
      ]
    | NewArray (n, e) ->
        SEQ [
          gen_expr e;
          CONST 4;
          OFFSET;
          LOADW;
          CONST 1;
          BINOP Plus;
          CONST 4;
          BINOP Times;
          GLOBAL (n.x_name ^ ".%desc");
          GLOBAL "lib.new";
          CALLW 2;
          DUP 0;
          GLOBAL (n.x_name ^ ".%desc");
          SWAP;
          STOREW;
        ]
    | Parent -> SEQ [LOCAL 12; LOADW]

and gen_cond tlab flab test = 
  SEQ [
    gen_expr test;
    CONST 4;
    OFFSET;
    LOADW;
    CONST 0;
    JUMPC (Neq, tlab);
    JUMP flab
  ]

and gen_assigment e1 e2 =
  let v = gen_expr e2 in
  match e1.e_guts with
  | Name n -> SEQ [v; gen_addr n; STOREW]
  | Property (e, n) -> SEQ[v; gen_expr e; gen_addr n; STOREW]
  | Sub (e3, e4) ->
    SEQ [
      v;
      gen_expr e3;
      gen_expr e4;
      CONST 4;
      OFFSET;
      LOADW;
      CONST 1;
      BINOP Plus;
      CONST 4;
      BINOP Times;
      OFFSET;
      STOREW;
    ]
  | _ -> raise InvalidAssigment

and gen_stmt s = 
  match s with
  | Assign (e1, e2) -> gen_assigment e1 e2
  | Delc (n, _, e) -> SEQ [gen_expr e; gen_addr n; STOREW]
  | Call e  -> gen_expr e
  | Return r -> 
    begin
      match r with
      | Some e -> SEQ [gen_expr e; RETURN 1]
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

and gen_method classname m = 
  SEQ [
    PROC (classname ^ "." ^ m.m_name.x_name, m.m_size, 0, 0);
    gen_stmt m.m_body;
    END
  ]

and gen_methods c = SEQ (List.map (gen_method c.c_name.x_name) c.c_methods)

and gen_class c =
  SEQ [
    DEFINE (c.c_name.x_name ^ ".%desc");
    WORD (DEC 0);
    WORD (SYMBOL (c.c_name.x_name ^ ".%anc"));
    WORD (DEC (List.length c.c_methods));
    SEQ (List.map (fun m -> WORD (SYMBOL (c.c_name.x_name ^ "." ^ m.m_name.x_name))) c.c_methods);
    DEFINE (c.c_name.x_name ^ ".%anc");
    SEQ (List.map (fun c -> WORD (SYMBOL (c.c_name.x_name ^ ".%desc"))) c.c_ancestors)
  ]

and gen_program (Program(cs)) =
  SEQ [
    SEQ lib_code;
    SEQ (List.map gen_methods cs);
    SEQ (List.map gen_class cs);
    PROC ("MAIN", 0, 0, 0);
    GLOBAL !mainMethod;
    CALLW 0;
    RETURN 0;
    END;
  ]

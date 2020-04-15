open Syntax.Tree
open Syntax.Keiko
open Errors
open Printf
open Lib.Int
open Lib.Out

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
  | Constant x ->
      SEQ [
        CONST 8;
        GLOBAL "Integer.%desc";
        GLOBAL "lib.new";
        CALLW 2;
        DUP 0;
        CONST x;
        SWAP;
        CONST 4;
        OFFSET;
        STOREW;
        DUP 0;
        GLOBAL "Integer.%desc";
        SWAP;
        STOREW;
      ]
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
          SEQ [ 
            DUP 0; 
            LOADW;
            gen_addr m;
            LOADW;
            CALLW (List.length args + 1)
          ]
      ]
  | Property (e1, n) -> SEQ [gen_expr e1; gen_addr n; LOADW]
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

and gen_assigment e1 e2 =
  let v = gen_expr e2 in
  match e1.e_guts with
  | Name n -> SEQ [v; gen_addr n; STOREW]
  | Property (e, n) -> SEQ[v; gen_expr e; gen_addr n; STOREW]
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
  | Seq ss -> SEQ (List.map gen_stmt ss)

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
    WORD (DEC (List.length c.c_methods));
    SEQ (List.map (fun m -> WORD (SYMBOL (c.c_name.x_name ^ "." ^ m.m_name.x_name))) c.c_methods)
  ]

and gen_program (Program(cs)) =
  SEQ [
    out_code;
    integer_code;
    SEQ (List.map gen_methods cs);
    SEQ (List.map gen_class cs);
    PROC ("MAIN", 0, 0, 0);
    GLOBAL !mainMethod;
    CALLW 0;
    RETURN 0;
    END;
  ]

open Syntax.Tree
open Keiko
open Printf

let me_pointer = 4
and v_table = -4

let get_size d =
  match d.d_type with
  | ClassType c -> c.c_size
  | _ -> printf "Error: Must use New with Class name"; exit 1


let gen_addr n =
  match n.x_def.d_kind with
  | ClassDef -> SEQ [GLOBAL (n.x_name ^ ".%desc")]
  | VariableDef off -> SEQ [LOCAL off]
  | PropertyDef off -> SEQ [CONST off; OFFSET]
  | MethodDef index -> SEQ [CONST (v_table + index); OFFSET]

let rec gen_expr e = 
  match e.e_guts with
  | Name n ->
    begin
      match n.x_def.d_kind with
      | ClassDef -> SEQ [GLOBAL n.x_name]
      | _ -> SEQ [gen_addr n; LOAD 4]
    end
  | MethodCall (e1, m, args) ->
      SEQ [ 
        SEQ (List.map gen_expr (List.rev args));
        gen_expr e1; 
        gen_addr m; LOAD 4;
        CALLW (List.length args)
      ]
  | Property (e1, n) -> SEQ [gen_expr e; gen_addr n; LOAD 4]
  | New n ->  SEQ [CONST (get_size n.x_def); GLOBAL (n.x_name ^ "%.desc"); GLOBAL "NEW"; CALLW 2]

and gen_assigment e1 e2 =
  let v = gen_expr e2 in
  match e1.e_guts with
  | Name n -> SEQ [v; gen_addr n; STORE 4]
  | Property (e, n) -> SEQ[v; gen_expr e; gen_addr n; STORE 4]
  | _ -> printf "Left-hand side of assigment must be a varible or property"; exit 1

and gen_stmt s = 
  match s with
  | Assign (e1, e2) -> gen_assigment e1 e2
  | Delc (n, _, e) -> SEQ [gen_expr e; gen_addr n; STORE 4]
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
    DEFINE (c.c_name.x_name ^ ".%dec");
    WORD (DEC 0);
    WORD (DEC (List.length c.c_methods));
    SEQ (List.map (fun m -> WORD (SYMBOL (c.c_name.x_name ^ "." ^ m.m_name.x_name))) c.c_methods)
  ]

and gen_program (Program(cs)) =
  SEQ [
    SEQ (List.map gen_methods cs);
    SEQ (List.map gen_class cs)
  ]

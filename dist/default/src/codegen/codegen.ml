(**open Syntax.Tree
open Keiko

let me_pointer = 4
and v_table = 4


let gen_addr n =
  match n.x_def.d_kind with
  | ClassDef -> SEQ [GLOBAL n.x_name]
  | VariableDef off -> SEQ [LOCAL off]
  | PropertyDef off -> SEQ [CONST off; OFFSET]
  | MethodDef index -> SEQ [CONST v_table; OFFSET; LOAD 4; CONST index; OFFSET]

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
        SEQ (List.map gen_arg (List.rev args));
        gen_expr e1; gen_addr m; LOAD 4;
        PCALL (0, 0)]
  | Property (e1, n) -> SEQ [gen_expr e; gen_addr n; LOAD 4]

and gen_arg x = SEQ [CONST 0]

and gen_assigment e1 e2 =
  let v = gen_expr e1 in
  match e1 with
  | Name n -> SEQ [v; gen_addr n; STORE]
  | Property (e, n) -> SEQ[v; gen_expr e; gen_addr n; STORE]
  | _ -> printf "Left-hand side of assigment must be a varible or property"; exit 1

and gen_stmt s = 
  match s with
  | Assign (e1, e2) -> gen_assigment e1 e2
  | Delc (n, _, e) -> SEQ [gen_expr e; gen_addr n; STORE]
  | Call e  -> gen_expr e
  | Return r -> 
    begin
      match r with
      | Some e -> SEQ [gen_expr e; RETURN 1]
      | None -> SEQ [RETURN 0]
    end
  | Seq ss -> SEQ (List.map gen_stmt ss)**)
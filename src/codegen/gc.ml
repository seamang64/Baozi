open Printf
open Syntax.Keiko
open Errors

(* Get 2^(n/4) - 1 *)
let rec gen_ones n =
  match n with
  | 0 -> 0
  | _ -> ((gen_ones (n-4)) lsl 1) + 1

(* Create the GC Map for a class *)
let gen_class_gc_map n =
  if n > 0 then sprintf "0x%X" (gen_ones (n+4))
  else "0"

(* Create the GC map for a procedure *)
let gen_proc_gc_map locals params =
  let local_off = 17 - (locals/4) and param_off = 20 in
    let gc = ((gen_ones locals) lsl local_off) lor ((gen_ones (params * 4)) lsl param_off) in
      if gc > 0 then gc+1 (* GC map must have 1 in the least-significant bit *)
      else 0

(* Create a GC map from an evaluation stack, for a STKMAP instruction *)
let gen_stack_gc n stack =
  let rec hex m stk =
      match stk with
      | [] -> 0
      | true::st ->
          if m < 1 then ((hex m st) lsl 1) + 1
          else (hex (m-1) st) lsl 1
      | false::st -> (hex (m-1) st) lsl 1
  in let h = hex n stack in
    if h == 0 then NOP
    else STKMAP ((h lsl 1) + 1) (* GC map must have 1 in the least-significant bit *)

(* Swap two values on the evaluation stack *)
let swap stk =
  match stk with
  | x::y::st -> y::x::st
  | _ -> raise InvalidExpression

(* duplicate a value on the evaluation stack *)
let dup n stk =
  let rec dup' n' stk' =
    match (n', stk') with
    | (0, s::_) -> s
    | (m, _::st) -> dup' (m-1) st
    | _ -> raise InvalidExpression
  in (dup' n stk)::stk

(* Simulate the evaluation stack and place any STKMAP instructions that are needed *)
let gen_stack_maps code =
  let rec gen_map code' stack =
    match code' with
    | CONST n -> (CONST n, false::stack)
    | GLOBAL s -> (GLOBAL s, false::stack)
    | LOCAL n -> (LOCAL n, true::stack)
    | LOADW -> (LOADW, true::(List.tl stack))
    | STOREW -> (STOREW, drop 2 stack)
    | CALLW n -> (* At a CALLW statement, we may need to add a STKMAP instruction *)
      let stkmap = gen_stack_gc n (List.tl stack) in
        (SEQ [stkmap; CALLW n], true::(drop (n+1) stack))
    | BINOP op -> (BINOP op, false::(drop 2 stack))
    | MONOP op -> (MONOP op, false::(List.tl stack))
    | OFFSET -> (OFFSET, true::(drop 2 stack))
    | SWAP -> (SWAP, swap stack)
    | DUP n -> (DUP n, dup n stack)
    | SEQ ss ->
        let (kcode, stk) = List.fold_left (fun (k', s) k -> let (nk, s') = gen_map k s in (k' @ [nk], s')) ([], stack) ss in
          (SEQ kcode, stk)
    | NOP -> (NOP, stack)
    | TYPE s -> (TYPE s, stack)
    | s -> print_keiko s; raise InvalidExpression
  in let (k, _) = gen_map code [] in k

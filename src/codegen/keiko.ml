type codelab = int
type symbol = string
let current_label = ref 0
let label () = incr current_label; !current_label
let reset_labels () = current_label := 0

type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not
  | Lsl | Lsr | Asr | BitAnd | BitOr | BitNot

type code =
    MODULE of symbol * int * int
  | IMPORT of symbol * int
  | ENDHDR
  | GLOVAR of symbol * int
  | DEFINE of symbol
  
  | CONST of int                (* Constant (value) *)
  | GLOBAL of symbol            (* Constant (symbol) *)
  | LOCAL of int                (* Local address (offset) *)
  | LOAD of int                 (* Load (size) *)
  | STORE of int                (* Store (size) *)
  | FIXCOPY                     (* Copy multiple values (size) *)
  | PCALL of int * int          (* Call procedure (nparams, rsize) *)
  | RETURN of int               (* Procedure return (rsize) *)
  | MONOP of op                 (* Perform unary operation (op) *)
  | BINOP of op                 (* Perform binary operation (op) *)
  | OFFSET                      (* Add address and offset *)
  | BOUND of int                (* Array bound check (line) *)
  | NCHECK of int               (* Null pointer check (line) *)
  | ERETURN of int              (* Failure to return (line) *)
  | LABEL of codelab            (* Set code label *)
  | JUMP of codelab             (* Unconditional branch (dest) *)
  | JUMPC of op * codelab       (* Conditional branch (cond, dest) *)
  | JCASE of codelab list       (* Jump table *)
  | LINE of int                 (* Line number *)

  | LDL of int * int            (* LDL (n, s) = LOCAL n / LOAD s *)
  | STL of int * int            (* STL (n, s) = LOCAL n / STORE s *)
  | LDG of symbol * int         (* LDG (x, s) = GLOBAL x / LOAD s *)
  | STG of symbol * int         (* STG (x, s) = GLOBAL x / STORE s *)
  | LDNW of int                 (* LDNW n = CONST n / OFFSET / LOAD 4 *)
  | STNW of int                 (* STNW n = CONST n / OFFSET / STORE 4 *)
  | LDI of int                  (* LDI s = CONST s / TIMES / OFFSET / LOAD s *)
  | STI of int                  (* STI s = CONST s / TIMES / OFFSET / STORE s *)
  | JUMPCZ of op * codelab      (* Conditional branch with zero (cond, dest) *)

  | SEQ of code list            (* Sequence of other instructions *)
  | NOP                         (* Null operation *)

let flatten_instructions x =
  let rec accum x ys =
    match x with
        SEQ xs -> List.fold_right accum xs ys
      | NOP -> ys
      | LINE n ->
          if n = 0 then
            ys
          else begin
            match ys with
                [] -> ys
              | LINE _ :: _ -> ys
              | _ -> LINE n :: ys
          end
      | _ -> x :: ys in
  accum x []
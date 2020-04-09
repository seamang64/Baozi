open Printf

type codelab = int
type symbol = string
let current_label = ref 0
let label () = incr current_label; !current_label
let reset_labels () = current_label := 0

type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not
  | Lsl | Lsr | Asr | BitAnd | BitOr | BitNot

type word =
    SYMBOL of symbol
  | DEC of int 

type code =
    MODULE of symbol * int * int
  | IMPORT of symbol * int
  | ENDHDR
  | GLOVAR of symbol * int
  | DEFINE of symbol
  | WORD of word
  | PROC of symbol * int * int * int
  
  | CONST of int                (* Constant (value) *)
  | GLOBAL of symbol            (* Constant (symbol) *)
  | LOCAL of int                (* Local address (offset) *)
  | LOAD of int                 (* Load (size) *)
  | STORE of int                (* Store (size) *)
  | FIXCOPY                     (* Copy multiple values (size) *)
  | CALLW of int                (* Call procedure (nparams, rsize) *)
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
  | SWAP
  | DUP
  | NOP                         (* Null operation *)
  | END

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

let print_op op =
  match op with
  | Plus -> "PLUS"
  | _ -> printf "Unrecongised Keiko"; exit 1

let rec print_keiko prog =
  match prog with
   | DEFINE s -> printf "\nDEFINE %s\n" s
   | WORD (SYMBOL s) -> printf "WORD %s\n" s
   | WORD (DEC n ) -> printf "WORD %d\n" n
   | PROC (d, n, i, f) -> printf "\nPROC %s %d %d %d\n" d n i f
   | CONST n -> printf "CONST %d\n" n
   | GLOBAL s -> printf "GLOBAL %s\n" s
   | LOCAL n -> printf "LOCAL %d\n" n
   | LOAD n -> printf "LOAD %d\n" n
   | STORE n -> printf "STORE %d\n" n
   | CALLW n -> printf "CALLW %d\n" n
   | RETURN n -> printf "RETURN %d\n" n 
   | BINOP op -> printf "BINOP %s\n" (print_op op)
   | OFFSET -> printf "OFFSET\n"
   | SEQ ss -> List.iter print_keiko ss
   | SWAP -> printf "SWAP\n"
   | DUP -> printf "DUP\n";
   | END -> printf "END\n"
   | NOP -> printf "NOP\n"
   | _ -> printf "Unrecongised Keiko"; exit 1
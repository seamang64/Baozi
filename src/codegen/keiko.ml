type label = int
type symbol = string

let current_label = ref 0

let label () = incr current_label; !current_label

let reset_labels () = current_label := 0

type keiko_type =
    PTR
  | VOID
  | SHORT
  | INT
  | LONG
  | FLOAT
  | DOUBLE
  | CHAR

let ktype_of_string str =
  match str with
      "char" -> CHAR
    | "short" -> SHORT
    | "int" -> INT
    | "long" -> LONG
    | "float" -> FLOAT
    | "double" -> DOUBLE
    | _ -> VOID

let int_of_ktype ktype =
  match ktype with
      CHAR -> 0
    | SHORT -> 1
    | INT -> 2
    | LONG -> 3
    | FLOAT -> 4
    | DOUBLE -> 5
    | _ -> -1

type unary_operator =
    UMINUS
  | NOT

type binary_operator =
    PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | AND
  | OR

type relation_operator =
    EQ
  | LT
  | GT
  | LTE
  | GTE
  | NEQ


type instruction =
    MODULE of keiko_symbol * int * int
  | IMPORT of keiko_symbol * int
  | ENDHDR

  | GLOVAR of keiko_symbol * int
  | DEFINE of keiko_symbol

  | GLOBAL of keiko_symbol
  | LOCAL of int
  | OFFSET

  | LOAD of keiko_type
  | STORE of keiko_type

  | LDL of keiko_type * int
  | STL of keiko_type * int
  | LDG of keiko_type * keiko_symbol
  | STG of keiko_type * keiko_symbol
  | LDNW of keiko_type * int
  | STNW of int

  | MONOP of keiko_type * unary_operator
  | BINOP of keiko_type * binary_operator
  | RELOP of keiko_type * relation_operator

  | LABEL of keiko_label
  | JUMP of keiko_label
  | JUMPC of keiko_type * relation_operator * keiko_label

  | JCASE of int
  | CASEL of keiko_label

  | ERROR of int * int

  | PROC of keiko_symbol * int * int * int
  | STKMAP of int * bool array
  | CALL of int * keiko_type
  | RETURN of int
  | END

  | DUP of int
  | POP of int
  | SWAP

  | LINE of int
  | COMMENT of string
  | NEWLINE
  | RAW of string

  | SEQ of instruction list
  | NOP

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
type token =
  | IDENT of (Tree.ident)
  | NUMBER of (int)
  | DOT
  | COLON
  | LCURL
  | RCURL
  | COMMA
  | LPAR
  | RPAR
  | EOF
  | NEW
  | PLUS
  | ASSIGN
  | BADTOKEN
  | AS
  | ARRAY
  | OF
  | PROPERTIES
  | METHOD
  | CLASSMETHOD
  | RETURN
  | BY
  | START
  | END
  | DEFINE
  | LEFTARROW
  | RIGHTARROW
  | DOUBLEARROW
  | UPARROW
  | LSQUARE
  | RSQUARE
  | NIL
  | ME

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program

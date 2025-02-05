{
  open Parser
  open Keiko
  open Source
  open Errors

  (* Create a hash type and add values to it *)
  let make_hash n ps =
    let t = Hashtbl.create n in
    List.iter (fun (k, v) -> Hashtbl.add t k v) ps; t

  let next_line lexbuf =
    incr lineno; Source.note_line !lineno lexbuf

  (* Keyword hash table *)
  let kwtable = make_hash 64
  [
    "Define", K_DEFINE;
    "Using", K_USING;

    "ClassMethod", K_CLASSMETHOD;
    "Main", K_MAIN;
    "Method", K_METHOD;
    "Properties", K_PROPERTIES;
    "ReplaceMethod", K_REPLACE;
    "ConstructorMethod", K_CONSTRUCTOR;

    "Me", K_ME;
    "My", K_ME;
    "Myself", K_ME;
    "Nil", K_NIL;
    "Parent", K_PARENT;

    "True", C_TRUE;
    "False", C_FALSE;
    "and", O_AND;
    "or", O_OR;
    "not", O_NOT;
    "InstanceOf", O_IS;
    "TypeOf", O_TYPEOF;

    "If", K_IF;
    "Then", K_THEN;
    "Else", K_ELSE;
    "While", K_WHILE;
    "For", K_FOR;
    "Step", K_STEP;
    "Test", K_TEST;
    "Cast", K_CAST;

    "Array", K_ARRAY;
    "New", K_NEW;
    "Of", K_OF;
    "Return", K_RETURN;
    "With", K_WITH;
  ]

  let idtable = Hashtbl.create 64

  (* Lookup a string in the keyword table *)
  let lookup s =
    try Hashtbl.find kwtable s with
      Not_found ->
        Hashtbl.replace idtable s ();
        IDENT s

  (* List of all string constants in the program *)
  let strtable = ref []

  let get_string s =
    let lab = gen_sym () in
      strtable := (lab, s)::!strtable;
      C_STR (lab, s)
}

let letter = ['A'-'Z''a'-'z']
let digit = ['0'-'9']
let qq = '"'
let notqq = [^'"''\n']

rule token = parse
    letter (letter | digit | '_')* as s
                        { lookup s }
  | digit+ as s         { C_NUMBER (int_of_string s) }
  | qq (notqq* as s) qq { get_string s }

  | "+"                 { O_PLUS }
  | "*"                 { O_TIMES }
  | "-"                 { O_MINUS}
  | "/"                 { O_DIV }
  | "%"                 { O_MOD }
  | "="                 { O_ASSIGN }
  | "->"                { O_RIGHTARROW }
  | "<-"                { O_LEFTARROW }
  | "=="                { O_EQUALS }
  | "/="                { O_NOTEQUALS }
  | "<"                 { O_LESSTHAN }
  | ">"                 { O_GREATERTHAN }
  | "<="                { O_LESSTHANEQ }
  | ">="                { O_GREATERTHANEQ }

  | "."                 { P_DOT }
  | ":"                 { P_COLON }
  | ","                 { P_COMMA }
  | "("                 { P_LPAR }
  | ")"                 { P_RPAR }
  | "["                 { P_LSQUARE }
  | "]"                 { P_RSQUARE }
  | "{"                 { P_LCURL }
  | "}"                 { P_RCURL }
  | "=>"                { P_DOUBLEARROW }
  | "$>"                { P_START }
  | "<$"                { P_END }

  | "!!"                { comment lexbuf; token lexbuf}
  | [' ''\t']+          { token lexbuf }
  | "\r"                { token lexbuf }
  | "\n"                { next_line lexbuf; token lexbuf }
  | _                   { BADTOKEN }
  | eof                 { EOF }

and comment =
  parse
    | "!!"              { () }
    | "\n"              { next_line lexbuf; comment lexbuf}
    | _                 { comment lexbuf }
    | eof               { raise UnexpectedEnd }
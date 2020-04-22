{
  open Parser

  let make_hash n ps =
    let t = Hashtbl.create n in
    List.iter (fun (k, v) -> Hashtbl.add t k v) ps; t

  let kwtable = make_hash 64
  [ 
    "Define", K_DEFINE;
    "As", K_AS; 
    "Array", K_ARRAY;
    "Of", K_OF; 

    "ClassMethod", K_CLASSMETHOD;
    "Main", K_MAIN; 
    "Method", K_METHOD;
    "Properties", K_PROPERTIES; 
    "ReplaceMethod", K_REPLACE;

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

    "If", K_IF;
    "Then", K_THEN;
    "Else", K_ELSE;
    "While", K_WHILE; 
    "For", K_FOR;
    "Step", K_STEP;
    "Test", K_TEST;
    
    "New", K_NEW; 
    "Return", K_RETURN
  ]

  let idtable = Hashtbl.create 64

  let lookup s =
    try Hashtbl.find kwtable s with
      Not_found -> 
        Hashtbl.replace idtable s ();
        IDENT s

  let get_vars () = 
    Hashtbl.fold (fun k () ks -> k::ks) idtable []
}

let letter = ['A'-'Z''a'-'z']
let digit = ['0'-'9']
let hexdigit = ['0'-'9''A'-'F']
let q = '\''
let qq = '"'
let notq = [^'\'''\n']
let notqq = [^'"''\n']

rule token = parse
    letter (letter | digit)* as s { lookup s }
  | digit+ as s         { NUMBER (int_of_string s)}

  | "+"                 { O_PLUS }
  | "="                 { O_ASSIGN }
  | "->"                { O_RIGHTARROW }
  | "<-"                { O_LEFTARROW }
  | "=="                { O_EQUALS }
  | "<"                 { O_LESSTHAN }

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

  | [' ''\t''\n''\r']+  { token lexbuf }
  | _                   { BADTOKEN }
  | eof                 { EOF }

{
  open Parser

  let make_hash n ps =
    let t = Hashtbl.create n in
    List.iter (fun (k, v) -> Hashtbl.add t k v) ps; t

  let kwtable = make_hash 64
      [ "As", AS; "Array", ARRAY; "BY", BY; "Class-Method", CLASSMETHOD;
        "Define", DEFINE; "Main", MAIN; "Method", METHOD; "Me", ME; "My", ME; "Myself", ME; "Nil", NIL; 
        "New", NEW; "Of", OF; "Properties", PROPERTIES; "Return", RETURN]

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
  | "+"                 { PLUS }
  | "."                 { DOT }
  | ":"                 { COLON }
  | ","                 { COMMA }
  | "("                 { LPAR }
  | ")"                 { RPAR }
  | "["                 { LSQUARE }
  | "]"                 { RSQUARE }
  | "{"                 { LCURL }
  | "}"                 { RCURL }
  | "="                 { ASSIGN }
  | "->"                { RIGHTARROW }
  | "<-"                { LEFTARROW }
  | "=>"                { DOUBLEARROW }
  | "$>"                { START }
  | "<$"                { END }
  | [' ''\t''\n''\r']+  { token lexbuf }
  | _                   { BADTOKEN }
  | eof                 { EOF }

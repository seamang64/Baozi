# 1 "src/syntax/lexer.mll"
 
  open Parser

  let make_hash n ps =
    let t = Hashtbl.create n in
    List.iter (fun (k, v) -> Hashtbl.add t k v) ps; t

  let kwtable = make_hash 64
      [ "As", AS; "Array", ARRAY; "BY", BY; "Class-Method", CLASSMETHOD;
        "Define", DEFINE; "Method", METHOD; "Me", ME; "My", ME; "Myself", ME; "Nil", NIL; 
        "New", NEW; "Of", OF; "Properties", PROPERTIES; "Return", RETURN]

  let idtable = Hashtbl.create 64

  let lookup s =
    try Hashtbl.find kwtable s with
      Not_found -> 
        Hashtbl.replace idtable s ();
        IDENT s

  let get_vars () = 
    Hashtbl.fold (fun k () ks -> k::ks) idtable []

# 26 "src/syntax/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\235\255\236\255\002\000\000\000\002\000\001\000\002\000\
    \244\255\245\255\246\255\247\255\248\255\249\255\250\255\251\255\
    \252\255\253\255\078\000\088\000\240\255\242\255\238\255\241\255\
    \239\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\018\000\019\000\019\000\019\000\012\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\001\000\000\000\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\000\000\003\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\003\000\000\000\004\000\000\000\022\000\000\000\
    \013\000\012\000\000\000\017\000\014\000\006\000\016\000\023\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\015\000\000\000\005\000\007\000\024\000\021\000\
    \020\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\011\000\000\000\010\000\000\000\000\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\009\000\000\000\008\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\003\000\003\000\000\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\003\000\255\255\000\000\255\255\005\000\255\255\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\004\000\006\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 34 "src/syntax/lexer.mll"
                                s
# 155 "src/syntax/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 34 "src/syntax/lexer.mll"
                                  ( lookup s )
# 159 "src/syntax/lexer.ml"

  | 1 ->
let
# 35 "src/syntax/lexer.mll"
              s
# 165 "src/syntax/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 35 "src/syntax/lexer.mll"
                        ( NUMBER (int_of_string s))
# 169 "src/syntax/lexer.ml"

  | 2 ->
# 36 "src/syntax/lexer.mll"
                        ( PLUS )
# 174 "src/syntax/lexer.ml"

  | 3 ->
# 37 "src/syntax/lexer.mll"
                        ( DOT )
# 179 "src/syntax/lexer.ml"

  | 4 ->
# 38 "src/syntax/lexer.mll"
                        ( COLON )
# 184 "src/syntax/lexer.ml"

  | 5 ->
# 39 "src/syntax/lexer.mll"
                        ( COMMA )
# 189 "src/syntax/lexer.ml"

  | 6 ->
# 40 "src/syntax/lexer.mll"
                        ( LPAR )
# 194 "src/syntax/lexer.ml"

  | 7 ->
# 41 "src/syntax/lexer.mll"
                        ( RPAR )
# 199 "src/syntax/lexer.ml"

  | 8 ->
# 42 "src/syntax/lexer.mll"
                        ( LSQUARE )
# 204 "src/syntax/lexer.ml"

  | 9 ->
# 43 "src/syntax/lexer.mll"
                        ( RSQUARE )
# 209 "src/syntax/lexer.ml"

  | 10 ->
# 44 "src/syntax/lexer.mll"
                        ( LCURL )
# 214 "src/syntax/lexer.ml"

  | 11 ->
# 45 "src/syntax/lexer.mll"
                        ( RCURL )
# 219 "src/syntax/lexer.ml"

  | 12 ->
# 46 "src/syntax/lexer.mll"
                        ( ASSIGN )
# 224 "src/syntax/lexer.ml"

  | 13 ->
# 47 "src/syntax/lexer.mll"
                        ( RIGHTARROW )
# 229 "src/syntax/lexer.ml"

  | 14 ->
# 48 "src/syntax/lexer.mll"
                        ( LEFTARROW )
# 234 "src/syntax/lexer.ml"

  | 15 ->
# 49 "src/syntax/lexer.mll"
                        ( DOUBLEARROW )
# 239 "src/syntax/lexer.ml"

  | 16 ->
# 50 "src/syntax/lexer.mll"
                        ( START )
# 244 "src/syntax/lexer.ml"

  | 17 ->
# 51 "src/syntax/lexer.mll"
                        ( END )
# 249 "src/syntax/lexer.ml"

  | 18 ->
# 52 "src/syntax/lexer.mll"
                        ( token lexbuf )
# 254 "src/syntax/lexer.ml"

  | 19 ->
# 53 "src/syntax/lexer.mll"
                        ( BADTOKEN )
# 259 "src/syntax/lexer.ml"

  | 20 ->
# 54 "src/syntax/lexer.mll"
                        ( EOF )
# 264 "src/syntax/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;


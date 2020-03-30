type token =
  | IDENT of (Tree.ident)
  | DOT
  | COLON
  | LCURL
  | RCURL
  | COMMA
  | LPAR
  | RPAR
  | EOF
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

open Parsing;;
let _ = parse_error;;
# 2 "src/syntax/parser.mly"
  open Tree
# 38 "src/syntax/parser.ml"
let yytransl_const = [|
  258 (* DOT *);
  259 (* COLON *);
  260 (* LCURL *);
  261 (* RCURL *);
  262 (* COMMA *);
  263 (* LPAR *);
  264 (* RPAR *);
    0 (* EOF *);
  265 (* ASSIGN *);
  266 (* BADTOKEN *);
  267 (* AS *);
  268 (* ARRAY *);
  269 (* OF *);
  270 (* PROPERTIES *);
  271 (* METHOD *);
  272 (* CLASSMETHOD *);
  273 (* RETURN *);
  274 (* BY *);
  275 (* START *);
  276 (* END *);
  277 (* DEFINE *);
  278 (* LEFTARROW *);
  279 (* RIGHTARROW *);
  280 (* DOUBLEARROW *);
  281 (* UPARROW *);
  282 (* LSQUARE *);
  283 (* RSQUARE *);
  284 (* NIL *);
  285 (* ME *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\005\000\005\000\
\006\000\006\000\008\000\008\000\007\000\007\000\009\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\013\000\013\000\004\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\008\000\006\000\010\000\000\000\004\000\
\000\000\002\000\010\000\010\000\000\000\005\000\001\000\002\000\
\003\000\003\000\005\000\002\000\001\000\001\000\001\000\002\000\
\004\000\001\000\007\000\003\000\001\000\000\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\033\000\001\000\000\000\032\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\010\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\006\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\029\000\000\000\000\000\015\000\000\000\000\000\000\000\023\000\
\000\000\000\000\011\000\000\000\000\000\000\000\024\000\000\000\
\012\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\025\000\000\000\000\000\000\000\000\000\000\000\027\000\031\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\064\000\015\000\021\000\026\000\022\000\
\059\000\060\000\061\000\062\000\085\000"

let yysindex = "\006\000\
\245\254\000\000\012\255\000\000\000\000\245\254\000\000\038\255\
\000\000\015\255\017\255\039\255\033\255\047\255\066\255\050\255\
\051\255\012\255\012\255\012\255\044\255\066\255\017\255\059\255\
\055\255\053\255\079\255\080\255\000\000\000\000\066\255\017\255\
\084\255\000\000\012\255\012\255\067\255\066\255\082\255\081\255\
\085\255\000\000\069\255\012\255\068\255\070\255\000\000\000\000\
\090\255\092\255\076\255\077\255\007\255\007\255\000\255\000\000\
\000\000\094\255\078\255\000\000\097\255\030\255\083\255\000\000\
\054\255\099\255\000\000\007\255\000\255\012\255\000\000\000\255\
\000\000\093\255\000\000\054\255\086\255\049\255\000\255\100\255\
\000\000\054\255\000\255\252\254\096\255\000\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\105\000\000\000\000\000\
\000\000\000\000\028\255\000\000\000\000\000\000\087\255\000\000\
\000\000\089\255\000\000\000\000\000\000\087\255\028\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\087\255\028\255\
\000\000\000\000\101\255\101\255\000\000\087\255\000\000\000\000\
\000\000\000\000\000\000\006\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\108\255\000\000\
\000\000\045\255\000\000\000\000\000\000\109\255\000\000\000\000\
\110\255\000\000\000\000\095\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\111\255\036\255\000\000\000\000\000\000\
\000\000\112\255\113\255\000\000\000\000\113\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\110\000\000\000\253\255\238\255\243\255\224\255\000\000\
\063\000\051\000\000\000\207\255\034\000"

let yytablesize = 120
let yytable = "\008\000\
\007\000\086\000\040\000\041\000\031\000\065\000\001\000\007\000\
\030\000\003\000\013\000\048\000\007\000\038\000\025\000\027\000\
\028\000\037\000\070\000\076\000\071\000\072\000\078\000\055\000\
\043\000\013\000\013\000\056\000\057\000\082\000\014\000\025\000\
\025\000\084\000\056\000\057\000\084\000\028\000\069\000\016\000\
\025\000\028\000\007\000\007\000\028\000\017\000\023\000\007\000\
\010\000\058\000\058\000\024\000\070\000\023\000\071\000\072\000\
\011\000\033\000\028\000\012\000\028\000\028\000\028\000\029\000\
\058\000\018\000\077\000\023\000\023\000\023\000\023\000\070\000\
\034\000\071\000\072\000\081\000\070\000\032\000\071\000\072\000\
\019\000\020\000\035\000\036\000\039\000\045\000\042\000\044\000\
\047\000\046\000\051\000\049\000\052\000\050\000\053\000\054\000\
\066\000\067\000\068\000\074\000\087\000\079\000\073\000\083\000\
\002\000\013\000\009\000\080\000\013\000\021\000\022\000\020\000\
\018\000\019\000\016\000\009\000\063\000\030\000\075\000\088\000"

let yycheck = "\003\000\
\001\001\006\001\035\000\036\000\023\000\055\000\001\000\001\001\
\022\000\021\001\005\001\044\000\001\001\032\000\018\000\019\000\
\020\000\031\000\023\001\069\000\025\001\026\001\072\000\017\001\
\038\000\020\001\012\001\028\001\029\001\079\000\014\001\035\000\
\036\000\083\000\028\001\029\001\086\000\002\001\009\001\001\001\
\044\000\006\001\015\001\016\001\009\001\013\001\002\001\020\001\
\011\001\053\000\054\000\001\001\023\001\009\001\025\001\026\001\
\019\001\003\001\023\001\022\001\025\001\026\001\027\001\020\001\
\068\000\019\001\070\000\023\001\019\001\025\001\026\001\023\001\
\020\001\025\001\026\001\027\001\023\001\019\001\025\001\026\001\
\015\001\016\001\004\001\004\001\001\001\005\001\020\001\006\001\
\020\001\005\001\001\001\024\001\001\001\024\001\019\001\019\001\
\003\001\020\001\002\001\001\001\005\001\009\001\020\001\004\001\
\000\000\005\001\020\001\022\001\020\001\002\001\002\001\002\001\
\002\001\002\001\020\001\006\000\054\000\005\001\068\000\086\000"

let yynames_const = "\
  DOT\000\
  COLON\000\
  LCURL\000\
  RCURL\000\
  COMMA\000\
  LPAR\000\
  RPAR\000\
  EOF\000\
  ASSIGN\000\
  BADTOKEN\000\
  AS\000\
  ARRAY\000\
  OF\000\
  PROPERTIES\000\
  METHOD\000\
  CLASSMETHOD\000\
  RETURN\000\
  BY\000\
  START\000\
  END\000\
  DEFINE\000\
  LEFTARROW\000\
  RIGHTARROW\000\
  DOUBLEARROW\000\
  UPARROW\000\
  LSQUARE\000\
  RSQUARE\000\
  NIL\000\
  ME\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'classes) in
    Obj.repr(
# 18 "src/syntax/parser.mly"
            ( Program(_1) )
# 214 "src/syntax/parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'oclass) in
    Obj.repr(
# 21 "src/syntax/parser.mly"
                   ( [_1] )
# 221 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'oclass) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classes) in
    Obj.repr(
# 22 "src/syntax/parser.mly"
                   ( _1 :: _2 )
# 229 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 25 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _4, false, _6, _7) )
# 239 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 26 "src/syntax/parser.mly"
                                                                   ( createClass(_2, "Object", false, _4, _5) )
# 248 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 27 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _6, true, _8, _9) )
# 258 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "src/syntax/parser.mly"
                                   ( [] )
# 264 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pairs) in
    Obj.repr(
# 31 "src/syntax/parser.mly"
                                   ( _3 )
# 271 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "src/syntax/parser.mly"
                      ( [] )
# 277 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'omethod) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methods) in
    Obj.repr(
# 35 "src/syntax/parser.mly"
                       ( _1 :: _2 )
# 285 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 39 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, _7, _9) )
# 295 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 41 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, _7, _9) )
# 305 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "src/syntax/parser.mly"
                    ( [] )
# 311 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pairs) in
    Obj.repr(
# 45 "src/syntax/parser.mly"
                                 ( Prop (_1, _3) :: _5 )
# 320 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 48 "src/syntax/parser.mly"
                     ( Seq _1 )
# 327 "src/syntax/parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 51 "src/syntax/parser.mly"
                       ( [_1] )
# 334 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 52 "src/syntax/parser.mly"
                       ( _1 :: _3)
# 342 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "src/syntax/parser.mly"
                                     ( Assign(_1, _3) )
# 350 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "src/syntax/parser.mly"
                                     ( Delc(_1, _3, _5) )
# 359 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "src/syntax/parser.mly"
                                     ( Return (Some _2) )
# 366 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "src/syntax/parser.mly"
                                     ( Return None )
# 372 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "src/syntax/parser.mly"
                                     ( Call(_1) )
# 379 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 62 "src/syntax/parser.mly"
                                   ( createExpr(Name _1) )
# 386 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 63 "src/syntax/parser.mly"
                                   ( createExpr(Parent(_1)) )
# 393 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 64 "src/syntax/parser.mly"
                                   ( createExpr(Sub(_1, _3)) )
# 401 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "src/syntax/parser.mly"
                                   ( createExpr(Nil) )
# 407 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 67 "src/syntax/parser.mly"
              ( createExpr(MethodCall(_1, _3, _6)) )
# 416 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 68 "src/syntax/parser.mly"
                                   ( createExpr(Property(_1, _3)) )
# 424 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "src/syntax/parser.mly"
                                   ( createExpr(Me) )
# 430 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "src/syntax/parser.mly"
                           ( [] )
# 436 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 73 "src/syntax/parser.mly"
                           ( _1 :: _3 )
# 444 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 76 "src/syntax/parser.mly"
              ( createName _1 )
# 451 "src/syntax/parser.ml"
               : 'name))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.program)

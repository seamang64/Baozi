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

open Parsing;;
let _ = parse_error;;
# 2 "src/syntax/parser.mly"
  open Tree
# 41 "src/syntax/parser.ml"
let yytransl_const = [|
  259 (* DOT *);
  260 (* COLON *);
  261 (* LCURL *);
  262 (* RCURL *);
  263 (* COMMA *);
  264 (* LPAR *);
  265 (* RPAR *);
    0 (* EOF *);
  266 (* NEW *);
  267 (* PLUS *);
  268 (* ASSIGN *);
  269 (* BADTOKEN *);
  270 (* AS *);
  271 (* ARRAY *);
  272 (* OF *);
  273 (* PROPERTIES *);
  274 (* METHOD *);
  275 (* CLASSMETHOD *);
  276 (* RETURN *);
  277 (* BY *);
  278 (* START *);
  279 (* END *);
  280 (* DEFINE *);
  281 (* LEFTARROW *);
  282 (* RIGHTARROW *);
  283 (* DOUBLEARROW *);
  284 (* UPARROW *);
  285 (* LSQUARE *);
  286 (* RSQUARE *);
  287 (* NIL *);
  288 (* ME *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\005\000\005\000\
\006\000\006\000\008\000\008\000\008\000\008\000\010\000\007\000\
\007\000\007\000\009\000\011\000\011\000\012\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\014\000\014\000\014\000\004\000\
\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\008\000\006\000\010\000\000\000\004\000\
\000\000\002\000\010\000\011\000\010\000\011\000\003\000\000\000\
\001\000\003\000\001\000\002\000\003\000\003\000\005\000\002\000\
\001\000\001\000\001\000\001\000\003\000\002\000\004\000\001\000\
\007\000\003\000\002\000\001\000\000\000\001\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\041\000\001\000\000\000\040\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\010\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\015\000\018\000\000\000\000\000\004\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\032\000\036\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\035\000\027\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\030\000\000\000\000\000\013\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\012\000\014\000\000\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\033\000\039\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\074\000\015\000\021\000\026\000\022\000\
\066\000\027\000\067\000\068\000\069\000\101\000"

let yysindex = "\012\000\
\246\254\000\000\043\255\000\000\000\000\246\254\000\000\087\255\
\000\000\014\255\034\255\051\255\050\255\037\255\244\254\052\255\
\075\255\043\255\043\255\043\255\061\255\244\254\034\255\064\255\
\089\255\074\255\093\255\101\255\108\255\000\000\000\000\244\254\
\034\255\113\255\000\000\043\255\043\255\043\255\094\255\244\254\
\000\000\000\000\110\255\112\255\000\000\096\255\095\255\097\255\
\000\000\004\255\078\255\098\255\115\255\103\255\117\255\017\255\
\104\255\017\255\105\255\000\000\043\255\000\255\000\000\000\000\
\124\255\106\255\000\000\127\255\079\255\017\255\109\255\017\255\
\000\000\000\000\028\255\130\255\000\000\017\255\000\255\000\255\
\043\255\000\000\000\255\111\255\000\000\114\255\121\255\000\000\
\028\255\028\255\116\255\059\255\000\000\000\000\000\255\131\255\
\000\000\028\255\000\255\253\254\129\255\000\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\138\000\000\000\000\000\
\000\000\000\000\092\255\000\000\000\000\000\000\119\255\000\000\
\000\000\120\255\000\000\000\000\000\000\119\255\092\255\000\000\
\000\000\000\000\005\255\000\000\000\000\000\000\000\000\119\255\
\092\255\000\000\000\000\039\255\133\255\133\255\000\000\119\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\137\255\000\000\000\000\
\070\255\000\000\000\000\000\000\141\255\000\000\000\000\000\000\
\000\000\000\000\142\255\000\000\000\000\123\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\065\255\144\255\035\255\000\000\000\000\000\000\000\000\000\000\
\000\000\145\255\143\255\146\255\000\000\143\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\144\000\000\000\253\255\027\000\246\255\066\000\000\000\
\022\000\000\000\073\000\000\000\197\255\051\000"

let yytablesize = 153
let yytable = "\008\000\
\007\000\060\000\075\000\102\000\052\000\019\000\020\000\079\000\
\053\000\061\000\017\000\031\000\001\000\003\000\025\000\028\000\
\029\000\007\000\060\000\089\000\090\000\039\000\081\000\092\000\
\082\000\083\000\061\000\017\000\013\000\046\000\063\000\064\000\
\025\000\025\000\025\000\098\000\062\000\034\000\079\000\100\000\
\034\000\034\000\100\000\007\000\016\000\034\000\034\000\063\000\
\064\000\032\000\014\000\016\000\065\000\081\000\065\000\082\000\
\083\000\073\000\018\000\040\000\034\000\016\000\034\000\034\000\
\034\000\017\000\065\000\029\000\065\000\079\000\029\000\029\000\
\027\000\023\000\065\000\024\000\029\000\091\000\054\000\071\000\
\027\000\027\000\055\000\030\000\081\000\033\000\082\000\083\000\
\097\000\079\000\080\000\084\000\034\000\086\000\029\000\027\000\
\035\000\027\000\027\000\036\000\010\000\042\000\043\000\044\000\
\081\000\037\000\082\000\083\000\011\000\007\000\007\000\012\000\
\038\000\041\000\007\000\047\000\045\000\048\000\049\000\056\000\
\057\000\050\000\059\000\051\000\058\000\070\000\072\000\076\000\
\077\000\078\000\087\000\085\000\095\000\093\000\103\000\099\000\
\094\000\002\000\016\000\025\000\096\000\009\000\016\000\026\000\
\024\000\020\000\022\000\023\000\037\000\009\000\088\000\038\000\
\104\000"

let yycheck = "\003\000\
\001\001\002\001\062\000\007\001\001\001\018\001\019\001\011\001\
\005\001\010\001\006\001\022\000\001\000\024\001\018\000\019\000\
\020\000\001\001\002\001\079\000\080\000\032\000\026\001\083\000\
\028\001\029\001\010\001\023\001\015\001\040\000\031\001\032\001\
\036\000\037\000\038\000\095\000\020\001\003\001\011\001\099\000\
\006\001\007\001\102\000\001\001\006\001\011\001\012\001\031\001\
\032\001\023\000\017\001\001\001\056\000\026\001\058\000\028\001\
\029\001\061\000\022\001\033\000\026\001\023\001\028\001\029\001\
\030\001\016\001\070\000\003\001\072\000\011\001\006\001\007\001\
\003\001\022\001\078\000\001\001\012\001\081\000\001\001\058\000\
\011\001\012\001\005\001\023\001\026\001\022\001\028\001\029\001\
\030\001\011\001\012\001\070\000\004\001\072\000\030\001\026\001\
\023\001\028\001\029\001\007\001\014\001\036\000\037\000\038\000\
\026\001\005\001\028\001\029\001\022\001\018\001\019\001\025\001\
\005\001\001\001\023\001\006\001\023\001\006\001\023\001\022\001\
\006\001\027\001\006\001\027\001\022\001\022\001\022\001\004\001\
\023\001\003\001\001\001\023\001\012\001\023\001\006\001\005\001\
\023\001\000\000\006\001\003\001\025\001\023\001\023\001\003\001\
\003\001\023\001\003\001\003\001\006\001\006\000\078\000\006\001\
\102\000"

let yynames_const = "\
  DOT\000\
  COLON\000\
  LCURL\000\
  RCURL\000\
  COMMA\000\
  LPAR\000\
  RPAR\000\
  EOF\000\
  NEW\000\
  PLUS\000\
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
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'classes) in
    Obj.repr(
# 19 "src/syntax/parser.mly"
            ( Program(_1) )
# 241 "src/syntax/parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'oclass) in
    Obj.repr(
# 22 "src/syntax/parser.mly"
                   ( [_1] )
# 248 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'oclass) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classes) in
    Obj.repr(
# 23 "src/syntax/parser.mly"
                   ( _1 :: _2 )
# 256 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 26 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _4, false, _6, _7) )
# 266 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 27 "src/syntax/parser.mly"
                                                                   ( createClass(_2, "Object", false, _4, _5) )
# 275 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 28 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _6, true, _8, _9) )
# 285 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "src/syntax/parser.mly"
                                   ( [] )
# 291 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pairs) in
    Obj.repr(
# 32 "src/syntax/parser.mly"
                                   ( _3 )
# 298 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "src/syntax/parser.mly"
                      ( [] )
# 304 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'omethod) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methods) in
    Obj.repr(
# 36 "src/syntax/parser.mly"
                       ( _1 :: _2 )
# 312 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 40 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, TempType(_7), _9) )
# 322 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pairs) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 42 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, VoidType, _10) )
# 331 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 44 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, TempType(_7), _9) )
# 341 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pairs) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 46 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, VoidType, _10) )
# 350 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 49 "src/syntax/parser.mly"
                    ( Prop (_1, TempType(_3)) )
# 358 "src/syntax/parser.ml"
               : 'pair))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "src/syntax/parser.mly"
                    ( [] )
# 364 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pair) in
    Obj.repr(
# 53 "src/syntax/parser.mly"
                    ( [_1] )
# 371 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pair) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pairs) in
    Obj.repr(
# 54 "src/syntax/parser.mly"
                     ( _1 :: _3 )
# 379 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 57 "src/syntax/parser.mly"
                     ( Seq _1 )
# 386 "src/syntax/parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 60 "src/syntax/parser.mly"
                       ( [_1] )
# 393 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "src/syntax/parser.mly"
                       ( _1 :: _3)
# 401 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "src/syntax/parser.mly"
                                     ( Assign(_1, _3) )
# 409 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "src/syntax/parser.mly"
                                     ( Delc(_1, TempType(_3), _5) )
# 418 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "src/syntax/parser.mly"
                                     ( Return (Some _2) )
# 425 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "src/syntax/parser.mly"
                                     ( Return None )
# 431 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "src/syntax/parser.mly"
                                     ( Call(_1) )
# 438 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 71 "src/syntax/parser.mly"
                                   ( createExpr (Name _1) )
# 445 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 72 "src/syntax/parser.mly"
                                   ( createExpr (Constant _1))
# 452 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "src/syntax/parser.mly"
                                   ( createExpr (MethodCall(_1, createName "add", [_3])) )
# 460 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "src/syntax/parser.mly"
                                   ( createExpr (Parent _1) )
# 467 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "src/syntax/parser.mly"
                                   ( createExpr (Sub(_1, _3)) )
# 475 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "src/syntax/parser.mly"
                                   ( createExpr Nil )
# 481 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 78 "src/syntax/parser.mly"
              ( createExpr (MethodCall(_1, _3, _6)) )
# 490 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 79 "src/syntax/parser.mly"
                                   ( createExpr (Property(_1, _3)) )
# 498 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 81 "src/syntax/parser.mly"
              ( createExpr (New _2) )
# 505 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "src/syntax/parser.mly"
                                   ( createExpr(Me) )
# 511 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "src/syntax/parser.mly"
                           ( [] )
# 517 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "src/syntax/parser.mly"
                           ( [_1] )
# 524 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 87 "src/syntax/parser.mly"
                           ( _1 :: _3 )
# 532 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 90 "src/syntax/parser.mly"
              ( createName _1 )
# 539 "src/syntax/parser.ml"
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

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
  | NEW
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
# 39 "src/syntax/parser.ml"
let yytransl_const = [|
  258 (* DOT *);
  259 (* COLON *);
  260 (* LCURL *);
  261 (* RCURL *);
  262 (* COMMA *);
  263 (* LPAR *);
  264 (* RPAR *);
    0 (* EOF *);
  265 (* NEW *);
  266 (* ASSIGN *);
  267 (* BADTOKEN *);
  268 (* AS *);
  269 (* ARRAY *);
  270 (* OF *);
  271 (* PROPERTIES *);
  272 (* METHOD *);
  273 (* CLASSMETHOD *);
  274 (* RETURN *);
  275 (* BY *);
  276 (* START *);
  277 (* END *);
  278 (* DEFINE *);
  279 (* LEFTARROW *);
  280 (* RIGHTARROW *);
  281 (* DOUBLEARROW *);
  282 (* UPARROW *);
  283 (* LSQUARE *);
  284 (* RSQUARE *);
  285 (* NIL *);
  286 (* ME *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\005\000\005\000\
\006\000\006\000\008\000\008\000\008\000\008\000\010\000\007\000\
\007\000\007\000\009\000\011\000\011\000\012\000\012\000\012\000\
\012\000\012\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\014\000\014\000\014\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\008\000\006\000\010\000\000\000\004\000\
\000\000\002\000\010\000\011\000\010\000\011\000\003\000\000\000\
\001\000\003\000\001\000\002\000\003\000\003\000\005\000\002\000\
\001\000\001\000\001\000\002\000\004\000\001\000\007\000\003\000\
\002\000\001\000\000\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\039\000\001\000\000\000\038\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\010\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\015\000\018\000\000\000\000\000\004\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\030\000\034\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\033\000\
\027\000\000\000\000\000\011\000\000\000\000\000\000\000\028\000\
\000\000\000\000\013\000\000\000\000\000\021\000\000\000\000\000\
\000\000\012\000\014\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\031\000\037\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\073\000\015\000\021\000\026\000\022\000\
\065\000\027\000\066\000\067\000\068\000\098\000"

let yysindex = "\004\000\
\004\255\000\000\045\255\000\000\000\000\004\255\000\000\247\254\
\000\000\015\255\039\255\057\255\049\255\047\255\080\255\055\255\
\076\255\045\255\045\255\045\255\058\255\080\255\039\255\064\255\
\085\255\071\255\092\255\095\255\096\255\000\000\000\000\080\255\
\039\255\100\255\000\000\045\255\045\255\045\255\081\255\080\255\
\000\000\000\000\098\255\099\255\000\000\084\255\082\255\083\255\
\000\000\046\255\061\255\086\255\104\255\090\255\106\255\001\255\
\093\255\001\255\094\255\045\255\011\255\000\000\000\000\109\255\
\097\255\000\000\113\255\063\255\001\255\101\255\001\255\000\000\
\000\000\238\254\115\255\000\000\001\255\011\255\045\255\000\000\
\011\255\102\255\000\000\103\255\107\255\000\000\238\254\105\255\
\067\255\000\000\000\000\011\255\116\255\000\000\238\254\011\255\
\254\254\114\255\011\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\121\000\000\000\000\000\
\000\000\000\000\027\255\000\000\000\000\000\000\108\255\000\000\
\000\000\110\255\000\000\000\000\000\000\108\255\027\255\000\000\
\000\000\000\000\002\255\000\000\000\000\000\000\000\000\108\255\
\027\255\000\000\000\000\008\255\120\255\120\255\000\000\108\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\124\255\000\000\000\000\059\255\
\000\000\000\000\000\000\125\255\000\000\000\000\000\000\000\000\
\000\000\128\255\000\000\000\000\111\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\131\255\054\255\
\000\000\000\000\000\000\000\000\000\000\000\000\132\255\130\255\
\133\255\000\000\130\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\130\000\000\000\253\255\019\000\005\000\034\000\000\000\
\236\255\000\000\060\000\000\000\196\255\040\000"

let yytablesize = 139
let yytable = "\008\000\
\074\000\007\000\010\000\099\000\001\000\079\000\017\000\080\000\
\081\000\060\000\011\000\007\000\016\000\012\000\025\000\028\000\
\029\000\087\000\061\000\060\000\089\000\079\000\017\000\080\000\
\081\000\003\000\031\000\013\000\016\000\062\000\063\000\095\000\
\025\000\025\000\025\000\097\000\039\000\070\000\097\000\062\000\
\063\000\032\000\007\000\007\000\046\000\007\000\052\000\007\000\
\082\000\053\000\084\000\040\000\064\000\014\000\064\000\032\000\
\072\000\016\000\032\000\032\000\027\000\054\000\017\000\032\000\
\055\000\064\000\018\000\064\000\027\000\042\000\043\000\044\000\
\078\000\064\000\023\000\088\000\024\000\032\000\030\000\032\000\
\032\000\032\000\027\000\033\000\027\000\027\000\079\000\034\000\
\080\000\081\000\079\000\035\000\080\000\081\000\094\000\019\000\
\020\000\036\000\037\000\038\000\041\000\045\000\047\000\048\000\
\049\000\056\000\050\000\051\000\057\000\058\000\059\000\075\000\
\069\000\071\000\077\000\085\000\092\000\076\000\100\000\096\000\
\002\000\083\000\090\000\091\000\016\000\025\000\026\000\093\000\
\009\000\024\000\016\000\020\000\022\000\023\000\035\000\009\000\
\086\000\036\000\101\000"

let yycheck = "\003\000\
\061\000\001\001\012\001\006\001\001\000\024\001\005\001\026\001\
\027\001\009\001\020\001\001\001\005\001\023\001\018\000\019\000\
\020\000\078\000\018\001\009\001\081\000\024\001\021\001\026\001\
\027\001\022\001\022\000\013\001\021\001\029\001\030\001\092\000\
\036\000\037\000\038\000\096\000\032\000\058\000\099\000\029\001\
\030\001\023\000\016\001\017\001\040\000\001\001\001\001\021\001\
\069\000\004\001\071\000\033\000\056\000\015\001\058\000\002\001\
\060\000\001\001\005\001\006\001\002\001\001\001\014\001\010\001\
\004\001\069\000\020\001\071\000\010\001\036\000\037\000\038\000\
\010\001\077\000\020\001\079\000\001\001\024\001\021\001\026\001\
\027\001\028\001\024\001\020\001\026\001\027\001\024\001\003\001\
\026\001\027\001\024\001\021\001\026\001\027\001\028\001\016\001\
\017\001\006\001\004\001\004\001\001\001\021\001\005\001\005\001\
\021\001\020\001\025\001\025\001\005\001\020\001\005\001\003\001\
\020\001\020\001\002\001\001\001\010\001\021\001\005\001\004\001\
\000\000\021\001\021\001\021\001\005\001\002\001\002\001\023\001\
\021\001\002\001\021\001\021\001\002\001\002\001\005\001\006\000\
\077\000\005\001\099\000"

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
# 229 "src/syntax/parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'oclass) in
    Obj.repr(
# 21 "src/syntax/parser.mly"
                   ( [_1] )
# 236 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'oclass) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classes) in
    Obj.repr(
# 22 "src/syntax/parser.mly"
                   ( _1 :: _2 )
# 244 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 25 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _4, false, _6, _7) )
# 254 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 26 "src/syntax/parser.mly"
                                                                   ( createClass(_2, "Object", false, _4, _5) )
# 263 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 27 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _6, true, _8, _9) )
# 273 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "src/syntax/parser.mly"
                                   ( [] )
# 279 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pairs) in
    Obj.repr(
# 31 "src/syntax/parser.mly"
                                   ( _3 )
# 286 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "src/syntax/parser.mly"
                      ( [] )
# 292 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'omethod) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methods) in
    Obj.repr(
# 35 "src/syntax/parser.mly"
                       ( _1 :: _2 )
# 300 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 39 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, TempType(_7), _9) )
# 310 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pairs) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 41 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, VoidType, _10) )
# 319 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 43 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, TempType(_7), _9) )
# 329 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pairs) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 45 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, VoidType, _10) )
# 338 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 48 "src/syntax/parser.mly"
                    ( Prop (_1, TempType(_3)) )
# 346 "src/syntax/parser.ml"
               : 'pair))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "src/syntax/parser.mly"
                    ( [] )
# 352 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pair) in
    Obj.repr(
# 52 "src/syntax/parser.mly"
                    ( [_1] )
# 359 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pair) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pairs) in
    Obj.repr(
# 53 "src/syntax/parser.mly"
                     ( _1 :: _3 )
# 367 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 56 "src/syntax/parser.mly"
                     ( Seq _1 )
# 374 "src/syntax/parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 59 "src/syntax/parser.mly"
                       ( [_1] )
# 381 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 60 "src/syntax/parser.mly"
                       ( _1 :: _3)
# 389 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "src/syntax/parser.mly"
                                     ( Assign(_1, _3) )
# 397 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "src/syntax/parser.mly"
                                     ( Delc(_1, TempType(_3), _5) )
# 406 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "src/syntax/parser.mly"
                                     ( Return (Some _2) )
# 413 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "src/syntax/parser.mly"
                                     ( Return None )
# 419 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "src/syntax/parser.mly"
                                     ( Call(_1) )
# 426 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 70 "src/syntax/parser.mly"
                                   ( createExpr (Name _1) )
# 433 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "src/syntax/parser.mly"
                                   ( createExpr (Parent _1) )
# 440 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "src/syntax/parser.mly"
                                   ( createExpr (Sub(_1, _3)) )
# 448 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "src/syntax/parser.mly"
                                   ( createExpr Nil )
# 454 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 75 "src/syntax/parser.mly"
              ( createExpr (MethodCall(_1, _3, _6)) )
# 463 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 76 "src/syntax/parser.mly"
                                   ( createExpr (Property(_1, _3)) )
# 471 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 78 "src/syntax/parser.mly"
              ( createExpr (New _2) )
# 478 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "src/syntax/parser.mly"
                                   ( createExpr(Me) )
# 484 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "src/syntax/parser.mly"
                           ( [] )
# 490 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "src/syntax/parser.mly"
                           ( [_1] )
# 497 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 84 "src/syntax/parser.mly"
                           ( _1 :: _3 )
# 505 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 87 "src/syntax/parser.mly"
              ( createName _1 )
# 512 "src/syntax/parser.ml"
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

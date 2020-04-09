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
  | MAIN
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
# 42 "src/syntax/parser.ml"
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
  268 (* MAIN *);
  269 (* ASSIGN *);
  270 (* BADTOKEN *);
  271 (* AS *);
  272 (* ARRAY *);
  273 (* OF *);
  274 (* PROPERTIES *);
  275 (* METHOD *);
  276 (* CLASSMETHOD *);
  277 (* RETURN *);
  278 (* BY *);
  279 (* START *);
  280 (* END *);
  281 (* DEFINE *);
  282 (* LEFTARROW *);
  283 (* RIGHTARROW *);
  284 (* DOUBLEARROW *);
  285 (* UPARROW *);
  286 (* LSQUARE *);
  287 (* RSQUARE *);
  288 (* NIL *);
  289 (* ME *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\005\000\005\000\
\006\000\006\000\008\000\008\000\008\000\008\000\008\000\010\000\
\007\000\007\000\007\000\009\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\014\000\014\000\014\000\
\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\008\000\006\000\010\000\000\000\004\000\
\000\000\002\000\010\000\011\000\010\000\011\000\010\000\003\000\
\000\000\001\000\003\000\001\000\002\000\003\000\003\000\005\000\
\002\000\001\000\001\000\001\000\001\000\003\000\002\000\004\000\
\001\000\007\000\003\000\002\000\001\000\000\000\001\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\001\000\000\000\041\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\010\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\019\000\000\000\000\000\000\000\004\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\033\000\037\000\000\000\
\000\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\028\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\031\000\000\000\000\000\015\000\013\000\000\000\000\000\
\022\000\000\000\000\000\000\000\000\000\012\000\014\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\034\000\
\040\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\082\000\015\000\021\000\026\000\022\000\
\073\000\027\000\074\000\075\000\076\000\110\000"

let yysindex = "\008\000\
\242\254\000\000\062\255\000\000\000\000\242\254\000\000\080\255\
\000\000\025\255\054\255\078\255\064\255\070\255\035\255\077\255\
\109\255\062\255\062\255\087\255\088\255\035\255\054\255\090\255\
\110\255\091\255\111\255\112\255\114\255\115\255\000\000\000\000\
\035\255\054\255\120\255\000\000\062\255\062\255\116\255\062\255\
\092\255\035\255\000\000\000\000\117\255\096\255\119\255\000\000\
\102\255\099\255\123\255\101\255\000\000\097\255\124\255\100\255\
\108\255\126\255\113\255\118\255\127\255\012\255\121\255\012\255\
\012\255\122\255\000\000\062\255\037\255\000\000\000\000\130\255\
\125\255\000\000\132\255\067\255\012\255\128\255\129\255\012\255\
\000\000\000\000\060\255\136\255\000\000\012\255\037\255\037\255\
\062\255\000\000\037\255\131\255\000\000\000\000\133\255\134\255\
\000\000\060\255\060\255\135\255\021\255\000\000\000\000\037\255\
\137\255\000\000\060\255\037\255\046\255\140\255\037\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\138\000\000\000\000\000\
\000\000\000\000\240\254\000\000\000\000\000\000\138\255\000\000\
\000\000\139\255\000\000\000\000\000\000\138\255\240\254\000\000\
\000\000\000\000\036\255\000\000\000\000\000\000\000\000\000\000\
\138\255\240\254\000\000\000\000\040\255\142\255\000\000\142\255\
\000\000\138\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\147\255\000\000\000\000\055\255\
\000\000\000\000\000\000\148\255\000\000\000\000\000\000\000\000\
\000\000\000\000\151\255\000\000\000\000\141\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\255\153\255\255\254\000\000\000\000\000\000\000\000\
\000\000\000\000\155\255\154\255\158\255\000\000\154\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\133\000\000\000\253\255\033\000\241\255\071\000\000\000\
\027\000\000\000\054\000\000\000\188\255\032\000"

let yytablesize = 165
let yytable = "\008\000\
\083\000\035\000\007\000\007\000\035\000\035\000\032\000\007\000\
\001\000\035\000\003\000\035\000\007\000\067\000\025\000\028\000\
\030\000\041\000\098\000\099\000\030\000\068\000\101\000\030\000\
\030\000\035\000\049\000\035\000\035\000\035\000\030\000\087\000\
\069\000\025\000\025\000\107\000\025\000\007\000\067\000\109\000\
\013\000\018\000\109\000\070\000\071\000\017\000\068\000\089\000\
\030\000\090\000\091\000\106\000\111\000\019\000\020\000\033\000\
\087\000\028\000\072\000\018\000\072\000\072\000\007\000\017\000\
\081\000\028\000\042\000\028\000\070\000\071\000\087\000\014\000\
\089\000\072\000\090\000\091\000\072\000\087\000\016\000\088\000\
\017\000\028\000\072\000\028\000\028\000\100\000\089\000\007\000\
\090\000\091\000\078\000\079\000\018\000\089\000\010\000\090\000\
\091\000\057\000\029\000\023\000\060\000\058\000\011\000\092\000\
\061\000\012\000\095\000\044\000\045\000\024\000\047\000\031\000\
\034\000\035\000\036\000\048\000\038\000\037\000\039\000\040\000\
\043\000\046\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\059\000\062\000\063\000\066\000\084\000\086\000\064\000\
\096\000\002\000\009\000\097\000\065\000\108\000\113\000\077\000\
\080\000\112\000\104\000\017\000\085\000\026\000\027\000\093\000\
\094\000\025\000\102\000\023\000\103\000\024\000\000\000\038\000\
\105\000\009\000\017\000\039\000\021\000"

let yycheck = "\003\000\
\069\000\003\001\019\001\020\001\006\001\007\001\022\000\024\001\
\001\000\011\001\025\001\013\001\001\001\002\001\018\000\019\000\
\020\000\033\000\087\000\088\000\003\001\010\001\091\000\006\001\
\007\001\027\001\042\000\029\001\030\001\031\001\013\001\011\001\
\021\001\037\000\038\000\104\000\040\000\001\001\002\001\108\000\
\016\001\006\001\111\000\032\001\033\001\006\001\010\001\027\001\
\031\001\029\001\030\001\031\001\007\001\019\001\020\001\023\000\
\011\001\003\001\062\000\024\001\064\000\065\000\001\001\024\001\
\068\000\011\001\034\000\013\001\032\001\033\001\011\001\018\001\
\027\001\077\000\029\001\030\001\080\000\011\001\001\001\013\001\
\017\001\027\001\086\000\029\001\030\001\089\000\027\001\001\001\
\029\001\030\001\064\000\065\000\023\001\027\001\015\001\029\001\
\030\001\001\001\012\001\023\001\001\001\005\001\023\001\077\000\
\005\001\026\001\080\000\037\000\038\000\001\001\040\000\024\001\
\023\001\004\001\024\001\024\001\005\001\007\001\005\001\005\001\
\001\001\006\001\006\001\028\001\006\001\024\001\028\001\005\001\
\028\001\006\001\023\001\006\001\006\001\004\001\003\001\023\001\
\001\001\000\000\006\000\086\000\023\001\005\001\111\000\023\001\
\023\001\006\001\013\001\006\001\024\001\003\001\003\001\024\001\
\024\001\003\001\024\001\003\001\024\001\003\001\255\255\006\001\
\026\001\024\001\024\001\006\001\024\001"

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
  MAIN\000\
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
# 252 "src/syntax/parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'oclass) in
    Obj.repr(
# 22 "src/syntax/parser.mly"
                   ( [_1] )
# 259 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'oclass) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'classes) in
    Obj.repr(
# 23 "src/syntax/parser.mly"
                   ( _1 :: _2 )
# 267 "src/syntax/parser.ml"
               : 'classes))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 26 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _4, false, _6, _7) )
# 277 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 27 "src/syntax/parser.mly"
                                                                   ( createClass(_2, "Object", false, _4, _5) )
# 286 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Tree.ident) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'properties) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'methods) in
    Obj.repr(
# 28 "src/syntax/parser.mly"
                                                                   ( createClass(_2, _6, true, _8, _9) )
# 296 "src/syntax/parser.ml"
               : 'oclass))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "src/syntax/parser.mly"
                                   ( [] )
# 302 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'pairs) in
    Obj.repr(
# 32 "src/syntax/parser.mly"
                                   ( _3 )
# 309 "src/syntax/parser.ml"
               : 'properties))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "src/syntax/parser.mly"
                      ( [] )
# 315 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'omethod) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methods) in
    Obj.repr(
# 36 "src/syntax/parser.mly"
                       ( _1 :: _2 )
# 323 "src/syntax/parser.ml"
               : 'methods))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 40 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, TempType(_7), _9, true) )
# 333 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pairs) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 42 "src/syntax/parser.mly"
        ( createMethod(_2, false, _4, VoidType, _10, true) )
# 342 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'pairs) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Tree.ident) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 44 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, TempType(_7), _9, true) )
# 352 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'pairs) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 46 "src/syntax/parser.mly"
        ( createMethod(_2, true, _4, VoidType, _10, true) )
# 361 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 48 "src/syntax/parser.mly"
        ( createMethod(createName "Main", true, [], VoidType, _9, true) )
# 368 "src/syntax/parser.ml"
               : 'omethod))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 51 "src/syntax/parser.mly"
                    ( Prop (_1, TempType(_3)) )
# 376 "src/syntax/parser.ml"
               : 'pair))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "src/syntax/parser.mly"
                    ( [] )
# 382 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pair) in
    Obj.repr(
# 55 "src/syntax/parser.mly"
                    ( [_1] )
# 389 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pair) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pairs) in
    Obj.repr(
# 56 "src/syntax/parser.mly"
                     ( _1 :: _3 )
# 397 "src/syntax/parser.ml"
               : 'pairs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 59 "src/syntax/parser.mly"
                     ( Seq _1 )
# 404 "src/syntax/parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 62 "src/syntax/parser.mly"
                       ( [_1] )
# 411 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 63 "src/syntax/parser.mly"
                       ( _1 :: _3)
# 419 "src/syntax/parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "src/syntax/parser.mly"
                                     ( Assign(_1, _3) )
# 427 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Tree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "src/syntax/parser.mly"
                                     ( Delc(_1, TempType(_3), _5) )
# 436 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "src/syntax/parser.mly"
                                     ( Return (Some _2) )
# 443 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "src/syntax/parser.mly"
                                     ( Return None )
# 449 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "src/syntax/parser.mly"
                                     ( Call(_1) )
# 456 "src/syntax/parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 73 "src/syntax/parser.mly"
                                   ( createExpr (Name _1) )
# 463 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "src/syntax/parser.mly"
                                   ( createExpr (Constant _1))
# 470 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "src/syntax/parser.mly"
                                   ( createExpr (MethodCall(_1, createName "add", [_3])) )
# 478 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 76 "src/syntax/parser.mly"
                                   ( createExpr (Parent _1) )
# 485 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "src/syntax/parser.mly"
                                   ( createExpr (Sub(_1, _3)) )
# 493 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "src/syntax/parser.mly"
                                   ( createExpr Nil )
# 499 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'name) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 80 "src/syntax/parser.mly"
              ( createExpr (MethodCall(_1, _3, _6)) )
# 508 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 81 "src/syntax/parser.mly"
                                   ( createExpr (Property(_1, _3)) )
# 516 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 83 "src/syntax/parser.mly"
              ( createExpr (New _2) )
# 523 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "src/syntax/parser.mly"
                                   ( createExpr(Me) )
# 529 "src/syntax/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "src/syntax/parser.mly"
                           ( [] )
# 535 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "src/syntax/parser.mly"
                           ( [_1] )
# 542 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments) in
    Obj.repr(
# 89 "src/syntax/parser.mly"
                           ( _1 :: _3 )
# 550 "src/syntax/parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tree.ident) in
    Obj.repr(
# 92 "src/syntax/parser.mly"
              ( createName _1 )
# 557 "src/syntax/parser.ml"
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

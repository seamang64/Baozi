%{
  open Tree
%}

%token <Tree.ident> IDENT
%token <int> NUMBER

%token DOT COLON LCURL RCURL COMMA LPAR RPAR EOF NEW PLUS MAIN
%token ASSIGN EOF BADTOKEN AS ARRAY OF PROPERTIES METHOD CLASSMETHOD RETURN BY
%token START END DEFINE LEFTARROW RIGHTARROW DOUBLEARROW UPARROW LSQUARE RSQUARE NIL ME

%type <Tree.program> program

%start program

%%

program :
    classes { Program($1) } ;

classes :
    oclass         { [$1] }
  | oclass classes { $1 :: $2 } ;

oclass :
    DEFINE name LEFTARROW IDENT START properties methods END       { createClass($2, $4, false, $6, $7) } 
  | DEFINE name START properties methods END                       { createClass($2, "Object", false, $4, $5) }
  | DEFINE name AS ARRAY OF IDENT START properties methods END     { createClass($2, $6, true, $8, $9) } ;

properties :
    /* empty */                    { [] }
  | PROPERTIES START pairs END     { $3 } ;

methods :
    /* empty */       { [] }
  | omethod methods    { $1 :: $2 } ;

omethod :
    METHOD name LCURL pairs RCURL DOUBLEARROW IDENT START stmts END      
        { createMethod($2, false, $4, TempType($7), $9, true) }
  | METHOD name LCURL pairs RCURL DOUBLEARROW LCURL RCURL START stmts END
        { createMethod($2, false, $4, VoidType, $10, true) }
  | CLASSMETHOD name LCURL pairs RCURL DOUBLEARROW IDENT START stmts END 
        { createMethod($2, true, $4, TempType($7), $9, true) }
  | CLASSMETHOD name LCURL pairs RCURL DOUBLEARROW LCURL RCURL START stmts END
        { createMethod($2, true, $4, VoidType, $10, true) }
  | CLASSMETHOD MAIN LCURL RCURL DOUBLEARROW LCURL RCURL START stmts END
        { createMethod(createName "Main", true, [], VoidType, $9, true) }

pair:
  name COLON IDENT  { Prop ($1, TempType($3)) }

pairs:
    /* empty */     { [] }
  | pair            { [$1] }
  | pair COMMA pairs { $1 :: $3 } ;

stmts :
    stmt_list        { Seq $1 } ;

stmt_list :
    stmt DOT           { [$1] }
  | stmt DOT stmt_list { $1 :: $3} ;

stmt :
    expr ASSIGN expr                 { Assign($1, $3) }
  | name COLON IDENT ASSIGN expr     { Delc($1, TempType($3), $5) }
  | RETURN expr                      { Return (Some $2) }
  | RETURN                           { Return None }
  | expr                             { Call($1) } ;

expr:
    name                           { createExpr (Name $1) }
  | NUMBER                         { createExpr (Constant $1)}
  | expr PLUS expr                 { createExpr (MethodCall($1, createName "add", [$3])) }
  | expr UPARROW                   { createExpr (Parent $1) }
  | expr LSQUARE expr RSQUARE      { createExpr (Sub($1, $3)) }
  | NIL                            { createExpr Nil }
  | expr RIGHTARROW name LEFTARROW LCURL arguments RCURL
              { createExpr (MethodCall($1, $3, $6)) }
  | expr RIGHTARROW name           { createExpr (Property($1, $3)) }
  | NEW name
              { createExpr (New $2) }
  | ME                             { createExpr(Me) } ;

arguments :
  |  /* empty */           { [] }
  | expr                   { [$1] }  
  | expr COMMA arguments   { $1 :: $3 } ;

name :  
    IDENT     { createName $1 } ;
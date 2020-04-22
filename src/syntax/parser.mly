%{
  open Tree
%}

%token <Tree.ident> IDENT
%token <int> NUMBER

%token K_DEFINE
%token K_AS
%token K_ARRAY
%token K_OF

%token K_CLASSMETHOD
%token K_MAIN
%token K_METHOD
%token K_PROPERTIES
%token K_REPLACE

%token K_ME
%token K_NIL
%token K_NEW 
%token K_RETURN
%token K_PARENT

%token K_IF
%token K_THEN
%token K_ELSE
%token K_WHILE
%token K_FOR
%token K_STEP
%token K_TEST

%token C_TRUE
%token C_FALSE

%token O_PLUS 
%token O_ASSIGN 
%token O_RIGHTARROW 
%token O_LEFTARROW 
%token O_EQUALS
%token O_LESSTHAN
%token O_AND
%token O_OR
%token O_NOT

%token P_DOT 
%token P_COLON 
%token P_COMMA 
%token P_LPAR 
%token P_RPAR 
%token P_LSQUARE 
%token P_RSQUARE 
%token P_LCURL 
%token P_RCURL 
%token P_DOUBLEARROW 
%token P_START 
%token P_END

%token BADTOKEN
%token EOF

%type <Tree.program> program

%start program

%%

program :
  |  classes 
      { Program($1) } ;

classes :
  | oclass         
      { [$1] }
  | oclass classes 
      { $1 :: $2 } ;

oclass :
  | K_DEFINE name O_LEFTARROW IDENT P_START properties methods P_END       
      { createClass($2, (TempType $4), false, $6, $7) } 
  | K_DEFINE name P_START properties methods P_END                       
      { createClass($2, (TempType "Object"), false, $4, $5) }
  | K_DEFINE name K_AS K_ARRAY K_OF IDENT
      { createClass($2, (TempType $6), true, [], []) } ;

properties :
  | /* empty */                    
      { [] }
  | K_PROPERTIES P_START pairs P_END     
      { $3 } ;

methods :
  | /* empty */       
      { [] }
  | omethod methods    
      { $1 :: $2 } ;

omethod :
  | K_METHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW IDENT P_START stmts P_END      
      { createMethod($2, false, $4, TempType($7), $9, false, false) }
  | K_METHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod($2, false, $4, VoidType, $10, false, false) }
  | K_CLASSMETHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW IDENT P_START stmts P_END 
      { createMethod($2, true, $4, TempType($7), $9, false, false) }
  | K_CLASSMETHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod($2, true, $4, VoidType, $10, false, false) }
  | K_REPLACE name P_LCURL pairs P_RCURL P_DOUBLEARROW IDENT P_START stmts P_END      
      { createMethod($2, false, $4, TempType($7), $9, false, true) }
  | K_REPLACE name P_LCURL pairs P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod($2, false, $4, VoidType, $10, false, true) }
  | K_CLASSMETHOD K_MAIN P_LCURL P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod(createName "Main", true, [], VoidType, $9, true, false) }

pair:
  | name P_COLON IDENT  
      { Prop ($1, TempType($3)) }

pairs:
  | /* empty */     
      { [] }
  | pair            
      { [$1] }
  | pair P_COMMA pairs 
      { $1 :: $3 } ;

stmts :
  | stmt_list        
      { Seq $1 } ;

stmt_list :
  |  stmt           
      { [$1] }
  | stmt stmt_list 
      { $1 :: $2} ;

stmt :
  |  expr O_ASSIGN expr P_DOT                
      { Assign($1, $3) }
  | name P_COLON IDENT O_ASSIGN expr P_DOT    
      { Delc($1, TempType($3), $5) }
  | K_RETURN expr P_DOT                     
      { Return (Some $2) }
  | K_RETURN P_DOT                          
      { Return None }
  | expr P_DOT                             
      { Call($1) }
  | K_IF expr K_THEN P_START stmts P_END elses
      { IfStmt($2, $5, $7) }
  | K_WHILE expr P_START stmts P_END
      { WhileStmt($2, $4) }
  | K_FOR for_stmt K_STEP for_stmt K_TEST expr P_START stmts P_END
      { ForStmt($2, $4, $6, $8) } ;

for_stmt :
  | /* empty */
      { Nop } 
  | stmt
      { $1 } ;

elses :
  | /* empty */
      { Nop }
  | K_ELSE P_START stmts P_END
      { $3 };

expr:
  | name                           
      { createExpr (Name $1) }
  | NUMBER                         
      { createExpr (Constant ($1, TempType("Int"))) }
  | expr O_PLUS expr                 
      { createExpr (MethodCall($1, createName "add", [$3])) }
  | expr O_EQUALS expr                 
      { createExpr (MethodCall($1, createName "equals", [$3])) }
  | expr O_LESSTHAN expr                 
      { createExpr (MethodCall($1, createName "lessThan", [$3])) }
  | expr O_AND expr                 
      { createExpr (MethodCall($1, createName "and", [$3])) }
  | expr O_OR expr                 
      { createExpr (MethodCall($1, createName "or", [$3])) }
  | O_NOT expr                 
      { createExpr (MethodCall($2, createName "not", [])) }
  | expr P_LSQUARE expr P_RSQUARE      
      { createExpr (Sub($1, $3)) }
  | K_NIL                            
      { createExpr Nil }
  | expr O_RIGHTARROW name O_LEFTARROW P_LCURL arguments P_RCURL
      { createExpr (MethodCall($1, $3, $6)) }
  | expr O_RIGHTARROW name           
      { createExpr (Property($1, $3)) }
  | expr O_RIGHTARROW P_LSQUARE expr P_RSQUARE
      { createExpr (Sub($1, $4)) }
  | K_NEW name
      { createExpr (New $2) }
  | K_NEW name O_LEFTARROW P_LSQUARE expr P_RSQUARE
      { createExpr (NewArray($2, $5)) }
  | K_ME                             
      { createExpr (Name (createName "Me")) }
  | K_PARENT
      { createExpr Parent }
  | C_TRUE
      { createExpr (Constant (1, TempType("Bool"))) }
  | C_FALSE
      { createExpr (Constant (0, TempType("Bool"))) }

arguments :
  | /* empty */           
      { [] }
  | expr                   
      { [$1] }  
  | expr P_COMMA arguments   
      { $1 :: $3 } ;

name :  
  | IDENT     
      { createName $1 } ;
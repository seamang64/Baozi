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
%token K_K_MAIN
%token K_METHOD
%token K_K_PROPERTIES

%token K_ME
%token K_NIL
        
%token K_NEW 
%token K_RETURN

%token O_PLUS 
%token O_ASSIGN 
%token O_RIGHTARROW 
%token O_LEFTARROW 

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
%token P_P_START 
%token P_END

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
  | K_DEFINE name LEFTARROW IDENT P_START properties methods P_END       
      { createClass($2, $4, false, $6, $7) } 
  | K_DEFINE name P_START properties methods P_END                       
      { createClass($2, "Object", false, $4, $5) }
  | K_DEFINE name K_AS K_ARRAY K_OF IDENT P_START properties methods P_END     
      { createClass($2, $6, true, $8, $9) } ;

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
      { createMethod($2, false, $4, TempType($7), $9, false) }
  | K_METHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod($2, false, $4, VoidType, $10, false) }
  | K_CLASSMETHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW IDENT P_START stmts P_END 
      { createMethod($2, true, $4, TempType($7), $9, false) }
  | K_CLASSMETHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod($2, true, $4, VoidType, $10, false) }
  | K_CLASSMETHOD K_MAIN P_LCURL P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_P_START stmts P_END
      { createMethod(createName "Main", true, [], VoidType, $9, true) }

pair:
  | name P__COLON IDENT  
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
  |  stmt P_DOT           
      { [$1] }
  | stmt P_DOT stmt_list 
      { $1 :: $3} ;

stmt :
  |  expr O_ASSIGN expr                 
      { Assign($1, $3) }
  | name P_COLON IDENT ASSIGN expr     
      { Delc($1, TempType($3), $5) }
  | K_RETURN expr                      
      { Return (Some $2) }
  | K_RETURN                           
      { Return None }
  | expr                             
      { Call($1) } ;

expr:
  | name                           
      { createExpr (Name $1) }
  | NUMBER                         
      { createExpr (Constant $1)}
  | expr O_PLUS expr                 
      { createExpr (MethodCall($1, createName "add", [$3])) }
  | expr O_UPARROW                   
      { createExpr (Parent $1) }
  | expr P_LSQUARE expr P_RSQUARE      
      { createExpr (Sub($1, $3)) }
  | K_NIL                            
      { createExpr Nil }
  | expr O_RIGHTARROW name O_LEFTARROW P_LCURL arguments P_RCURL
      { createExpr (MethodCall($1, $3, $6)) }
  | expr O_RIGHTARROW name           
      { createExpr (Property($1, $3)) }
  | K_NEW name
      { createExpr (New $2) }
  | K_ME                             
      { createExpr(Me) } ;

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
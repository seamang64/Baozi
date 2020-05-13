%{
  open Tree
  open Source
%}

%token <Tree.ident> IDENT
%token <int> NUMBER
%token <Keiko.symbol * string> STR

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
%token O_TIMES
%token O_MINUS
%token O_DIV
%token O_MOD
%token O_ASSIGN
%token O_RIGHTARROW
%token O_LEFTARROW
%token O_EQUALS
%token O_NOTEQUALS
%token O_LESSTHAN
%token O_GREATERTHAN
%token O_LESSTHANEQ
%token O_GREATERTHANEQ
%token O_AND
%token O_OR
%token O_NOT
%token O_IS
%token O_TYPEOF

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

%left O_PLUS
%left O_TIMES
%left O_MINUS
%left O_DIV
%left O_MOD
%left O_ASSIGN
%left O_LEFTARROW
%left O_EQUALS
%left O_NOTEQUALS
%left O_LESSTHAN
%left O_GREATERTHAN
%left O_LESSTHANEQ
%left O_GREATERTHANEQ
%left O_AND
%left O_OR
%left O_IS
%nonassoc O_NOT
%nonassoc O_UMINUS
%nonassoc O_TYPEOF

%type <Tree.program> program

%start program

%%

program :
  |  classes
      { Program($1) } ;

classes :
  | cls
      { [$1] }
  | cls classes
      { $1 :: $2 } ;

cls :
  | K_DEFINE name parent P_START properties methods P_END
      { createClass($2, $3, $5, $6) }

parent :
  | /* empty */
      { TempType (Ident "Object") }
  | O_LEFTARROW IDENT
      { TempType (Ident $2) };

properties :
  | /* empty */
      { [] }
  | K_PROPERTIES P_START pairs P_END
      { $3 } ;

methods :
  | /* empty */
      { [] }
  | meth methods
      { $1 :: $2 } ;

meth :
  | K_METHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW mtype P_START stmts P_END
      { createMethod($2, false, $4, $7, $9, false, false) }
  | K_CLASSMETHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW mtype P_START stmts P_END
      { createMethod($2, true, $4, $7, $9, false, false) }
  | K_REPLACE name P_LCURL pairs P_RCURL P_DOUBLEARROW mtype P_START stmts P_END
      { createMethod($2, false, $4, $7, $9, false, true) }
  | K_CLASSMETHOD K_MAIN P_LCURL P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod(createName "Main", true, [], VoidType, $9, true, false) }

mtype :
  | otype
      { TempType $1 }
  | P_LCURL P_RCURL
      { VoidType }

otype :
  | IDENT
      {Ident $1 }
  | K_ARRAY K_OF otype
      { Array $3}

pair:
  | name P_COLON otype
      { Prop ($1, TempType $3) }

pairs:
  | /* empty */
      { [] }
  | pair
      { [$1] }
  | pair P_COMMA pairs
      { $1 :: $3 } ;

stmts :
  | stmt_list
      { seq $1 } ;

stmt_list :
  | stmt
      { [$1] }
  | stmt stmt_list
      { $1 :: $2} ;

stmt :
  | stmt1
      { createStmt $1 }

stmt1 :
  |  expr O_ASSIGN expr P_DOT
      { Assign($1, $3) }
  | name P_COLON otype O_ASSIGN expr P_DOT
      { Delc($1, TempType $3, $5) }
  | K_RETURN expr P_DOT
      { Return (Some $2) }
  | K_RETURN P_DOT
      { Return None }
  | expr P_DOT
      { Call($1) }
  | K_IF expr K_THEN body elses
      { IfStmt($2, $4, $5) }
  | K_WHILE expr P_DOT body
      { WhileStmt($2, $4) }
  | K_FOR for_stmt K_STEP for_stmt K_TEST expr P_DOT body
      { ForStmt($2, $4, $6, $8) } ;

body :
  | stmt
    { $1 }
  | P_START stmts P_END
    { $2 }

for_stmt :
  | /* empty */
      { createEmptyStmt Nop }
  | stmt
      { $1 } ;

elses :
  | /* empty */
      { createEmptyStmt Nop }
  | K_ELSE body
      { $2 };

expr:
  | name
      { Name $1 }
  | NUMBER
      { Constant ($1, TempType (Ident "Int")) }
  | STR
      { let (lab, s) = $1 in String (lab, s) }
  | O_TYPEOF expr
      { TypeOf $2 }
  | O_MINUS expr %prec O_UMINUS
      { MethodCall($2, createName "uminus", []) }
  | O_NOT expr
      { MethodCall($2, createName "not", []) }
  | expr O_PLUS expr
      { MethodCall($1, createName "add", [$3]) }
  | expr O_TIMES expr
      { MethodCall($1, createName "times", [$3]) }
  | expr O_MINUS expr
      { MethodCall($1, createName "sub", [$3]) }
  | expr O_DIV expr
      { MethodCall($1, createName "div", [$3]) }
  | expr O_MOD expr
      { MethodCall($1, createName "mod", [$3]) }
  | expr O_EQUALS expr
      { MethodCall($1, createName "equals", [$3]) }
  | expr O_NOTEQUALS expr
      { MethodCall($1, createName "notEquals", [$3]) }
  | expr O_LESSTHAN expr
      { MethodCall($1, createName "lessThan", [$3]) }
  | expr O_GREATERTHAN expr
      { MethodCall($1, createName "greaterThan", [$3]) }
  | expr O_LESSTHANEQ expr
      { MethodCall($1, createName "lessThanEq", [$3]) }
  | expr O_GREATERTHANEQ expr
      { MethodCall($1, createName "greaterThanEq", [$3]) }
  | expr O_AND expr
      { MethodCall($1, createName "and", [$3]) }
  | expr O_OR expr
      { MethodCall($1, createName "or", [$3]) }
  | expr O_IS expr
      { MethodCall($1, createName "Is", [$3]) }
  | expr P_LSQUARE expr P_RSQUARE
      { Sub($1, $3) }
  | K_NIL
      { Nil }
  | expr O_RIGHTARROW name O_LEFTARROW P_LCURL arguments P_RCURL
      { MethodCall($1, $3, $6) }
  | expr O_RIGHTARROW name
      { Property($1, $3) }
  | expr O_RIGHTARROW P_LSQUARE expr P_RSQUARE
      { Sub($1, $4) }
  | K_NEW name
      { New $2 }
  | K_NEW otype O_LEFTARROW P_LSQUARE expr P_RSQUARE
      { NewArray(createArrayName (TempType $2), $5) }
  | K_ME
      { Name (createName "Me") }
  | K_PARENT
      { Parent }
  | C_TRUE
      { Constant (1, TempType (Ident "Bool")) }
  | C_FALSE
      { Constant (0, TempType (Ident "Bool")) }
  | P_LPAR expr P_RPAR
      { $2 }

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
%{
  open Tree
%}

/* Token names use underscores to denote their "type" for clarity of reading
    - K : Keyword
    - P : Punctuation
    - O : Operator
    - C : Constant
*/

/* Primitives */
%token <Tree.ident> IDENT
%token <int> C_NUMBER
%token <Keiko.symbol * string> C_STR

/* Keywords */
%token K_DEFINE
%token K_AS
%token K_ARRAY
%token K_OF
%token K_USING
%token K_WITH

%token K_CLASSMETHOD
%token K_MAIN
%token K_METHOD
%token K_PROPERTIES
%token K_REPLACE
%token K_CONSTRUCTOR

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
%token K_CAST

/* Boolean Constants */
%token C_TRUE
%token C_FALSE

/* Operators */
%token O_PLUS
%token O_TIMES
%token O_MINUS
%token O_DIV
%token O_MOD
%token O_ASSIGN
%token O_RIGHTARROW
%token O_LEFTARROW
%token O_LEFTARROWSQUARE
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
%nonassoc O_UMINUS

/* Punctution */
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

/* Program */
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
  | K_DEFINE name parent generics P_START properties methods P_END
      { createClass($2, $3, $6, $7, $4) }

/* Generic types for class */
generics :
  | /* empty */
      { [] }
  | K_USING generic_list
      { $2 }

/* List of generic definitions */
generic_list :
  | IDENT
      { [createGeneric $1 (TempType (Ident "Object"))] }
  | IDENT O_LEFTARROW ttype
      { [createGeneric $1 (TempType $3)] }
  | IDENT P_COMMA generic_list
      { (createGeneric $1 (TempType (Ident "Object"))) :: $3 }
  | IDENT O_LEFTARROW ttype P_COMMA generic_list
      { (createGeneric $1 (TempType $3)) :: $5 }

/* Parent type of class */
parent :
  | /* empty */
      { TempType (Ident "Object") }
  | O_LEFTARROW ttype
      { TempType $2 };

/* Properties for class */
properties :
  | /* empty */
      { [] }
  | K_PROPERTIES P_START pairs P_END
      { $3 } ;

/* List of methods */
methods :
  | /* empty */
      { [] }
  | meth methods
      { $1 :: $2 } ;

meth :
  | K_CLASSMETHOD K_MAIN P_LCURL P_RCURL P_DOUBLEARROW P_LCURL P_RCURL P_START stmts P_END
      { createMethod(createName "Main", true, [], VoidType, $9, true, false) }
  | K_METHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW mtype P_START stmts P_END
      { createMethod($2, false, $4, $7, $9, false, false) }
  | K_CLASSMETHOD name P_LCURL pairs P_RCURL P_DOUBLEARROW mtype P_START stmts P_END
      { createMethod($2, true, $4, $7, $9, false, false) }
  | K_REPLACE name P_LCURL pairs P_RCURL P_DOUBLEARROW mtype P_START stmts P_END
      { createMethod($2, false, $4, $7, $9, false, true) }

/* Type of a method */
mtype :
  | ttype
      { TempType $1 }
  | P_LCURL P_RCURL
      { VoidType }

/* Temporary type */
ttype :
  | IDENT
      { Ident $1 }
  | K_ARRAY K_OF ttype
      { Array $3}
  | IDENT K_WITH P_LPAR type_list P_RPAR
      { Generic ($1, $4) }

/* List of types for generic type */
type_list :
  | ttype
      { [$1] }
  | ttype P_COMMA type_list
      { $1 :: $3 }

/* Pair of a name and a type */
pair:
  | name P_COLON ttype
      { Prop ($1, TempType $3) }

/* List of pairs */
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
  | name P_COLON ttype O_ASSIGN expr P_DOT
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

/* Body of if/for/while statments */
body :
  | stmt
    { $1 }
  | P_START stmts P_END
    { $2 }

/* Initial and step statments for for loop */
for_stmt :
  | /* empty */
      { createEmptyStmt Nop }
  | stmt
      { $1 } ;

/* else statment for if */
elses :
  | /* empty */
      { createEmptyStmt Nop }
  | K_ELSE body
      { $2 };

expr:
  | simple
      { $1 }
  | simple O_EQUALS simple
      { MethodCall($1, createName "equals", [$3]) }
  | simple O_NOTEQUALS simple
      { MethodCall($1, createName "notEquals", [$3]) }
  | simple O_LESSTHAN simple
      { MethodCall($1, createName "lessThan", [$3]) }
  | simple O_GREATERTHAN simple
      { MethodCall($1, createName "greaterThan", [$3]) }
  | simple O_LESSTHANEQ simple
      { MethodCall($1, createName "lessThanEq", [$3]) }
  | simple O_GREATERTHANEQ simple
      { MethodCall($1, createName "greaterThanEq", [$3]) }
  | simple O_AND simple
      { MethodCall($1, createName "and", [$3]) }
  | simple O_OR simple
      { MethodCall($1, createName "or", [$3]) }
  | simple O_IS simple
      { MethodCall($1, createName "InstanceOf", [$3]) }

simple :
  | term
      { $1 }
  | O_MINUS term %prec O_UMINUS
      { MethodCall($2, createName "uminus", []) }
  | simple O_PLUS term
      { MethodCall($1, createName "add", [$3]) }
  | simple O_MINUS term
      { MethodCall($1, createName "sub", [$3]) }
  | O_TYPEOF name
      { TypeOf $2 }

term :
  | factor
      { $1 }
  | term O_TIMES factor
      { MethodCall($1, createName "times", [$3]) }
  | term O_DIV factor
      { MethodCall($1, createName "div", [$3]) }
  | term O_MOD factor
      { MethodCall($1, createName "mod", [$3]) }

factor :
  | name
      { Name $1 }
  | C_NUMBER
      { Constant ($1, TempType (Ident "Int")) }
  | C_STR
      { let (lab, s) = $1 in String (lab, s) }
  | factor O_RIGHTARROW name O_LEFTARROW P_LCURL arguments P_RCURL
      { MethodCall ($1, $3, $6) }
  | factor O_RIGHTARROW name
      { Property($1, $3) }
  | factor O_RIGHTARROW P_LSQUARE expr P_RSQUARE
      { Sub($1, $4) }
  | K_CAST P_LPAR expr P_RPAR O_RIGHTARROW name
      { Cast($3, $6)}
  | O_NOT factor
      { MethodCall($2, createName "not", []) }
  | K_NIL
      { Nil }
  | K_NEW ttype O_LEFTARROWSQUARE expr P_RSQUARE
      { NewArray (createTypeName (TempType $2), $4) }
  | K_NEW ttype
      { New (createTypeName (TempType $2)) }
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
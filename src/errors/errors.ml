open Printf

exception UnknownName of string

exception DuplicateName of string

exception VoidOperation

exception UnannotatedName of string

exception InvalidNew of string

exception InvalidAssigment

exception TypeError of string * string

exception UnknownExpression

exception InvalidReturn

exception IncompleteStatement

exception IncorrectArgumentCount

exception InvalidSub

exception UnknownConstant

exception NoneKindError of string

exception IncorrectSyntaxError

let print_exception e =
  match e with
  | UnknownName n -> printf "Error: The name '%s' does not exist in the current context\n" n
  | DuplicateName n -> printf "Error: The name '%s' is a duplicate\n" n
  | VoidOperation -> printf "Error: Cannot apply '->' operand to type '{}'\n"
  | UnannotatedName n -> printf "Error: The name '%s' has not been annoted by the compiler\n" n
  | InvalidNew n -> printf "Error: Must apply 'New' to a Class name. '%s' is not a Class name\n" n
  | InvalidAssigment -> printf "Error: The left-had side of an assigment must be a variable or property\n"
  | TypeError (s1, s2) -> printf "Error: '%s' and '%s' are incompatible types\n" s1 s2
  | UnknownExpression -> printf "Error: Compiler does not handle expressions of this form\n"
  | InvalidReturn -> printf "Error: Incorrect return type\n"
  | IncompleteStatement -> printf "Error: Incomplete statement\n"
  | IncorrectArgumentCount -> printf "Error: Wrong number of arguments\n"
  | InvalidSub -> printf "Must use the [] notation with an array type\n"
  | UnknownConstant -> printf "Constant of unknown type\n"
  | NoneKindError n -> printf "%s has defintion kind of None\n" n
  | IncorrectSyntaxError -> printf "Incorrect syntax\n"
  | _ -> printf "Unknown Error\n";
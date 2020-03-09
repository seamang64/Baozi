open Syntax.Tree
open Printf

let rec getAllClasses classes =
	match classes with
	| c::cs -> printf "%s\n" c.c_name.x_name; c::(getAllClasses cs)
	| _ -> []

let getClassNames progam = 
	match progam with
	| Program(cs) -> getAllClasses cs
open Syntax.Tree
open Printf

let propertyOffset = 12;
let arguementOffset = 16;

let rec getAllClasses classes =
	match classes with
	| c::cs -> printf "%s\n" c.c_name.x_name; c.c_name.x_def <- Some ClassDef; c.c_name.x_name::(getAllClasses cs)
	| _ -> []

let rec findtype types o =
  match types with
  | t:ts -> if t.c_name.x_name == o then true;
            else findtype ts o;
  | _ -> false

let getClassNames progam = 
	match progam with
	| Program(cs) -> getAllClasses cs


let rec anotateMembers types classes =
  match  classes with
  | c::cs -> anotateProperties(types, c.c_properties, 0); anotateMethods(c.c_methods, 0); anotateMembers(types, cs)
  | _ -> ();

let rec anotateArguments types args index = 
  | (Prop(x, o))::props -> if findtype(types, o) then x.x_name.x_def <- Some (VariableDef(argumentOffset + index)); anotateArguments(types props, index+1);
                          else printf "Invalid Type"; exit 1;
  | _ -> ();

let rec anotateProperties types properties index =
  match properties with
  | (Prop(x, o))::props -> if findtype(types, o) then x.x_name.x_def <- Some (PropertyDef(propertyOffset + index)); anotateProperties(types props, index+1);
                          else printf "Invalid Type"; exit 1;
  | _ -> ();

let rec anotateMethods types methods index =
  match methods with
  | m:ms -> if (findtype(types, m.m_type)) then m.m_name.x_def <- Some (MethodDef(index)); anotateArguments(types, m.m_arguemnts, 0); anotateMethods(types, ms, index+1);
            else printf "Invalid Type": exit 1;
  | _ -> ();
open Syntax.Tree
open Printf

let propertyOffset = 12
let argumentOffset = 16

let rec getAllClasses classes classTable =
	match classes with
	| c::cs -> printf "%s\n" c.c_name.x_name; Hashtbl.add classTable c.c_name.x_name c; getAllClasses cs classTable
	| _ -> ()

let getClassNames progam = 
	match progam with
	| Program(cs) -> let ct = Hashtbl.create 100 in getAllClasses cs ct; ct

let rec anotateArguments args index = 
  match args with
  | (Prop(x, _))::props -> x.x_offset <- argumentOffset + index; anotateArguments props (index+1)
  | _ -> ()

let rec anotateProperties properties index =
  match properties with
  | (Prop(x, _))::props -> x.x_offset <- propertyOffset + index; anotateProperties props (index+1)
  | _ -> ()

let rec anotateMethods classname methods index =
  match methods with
  | m::ms -> m.m_name.x_offset <- index; 
            if m.m_static then anotateArguments m.m_arguments 1
            else anotateArguments m.m_arguments 0;
            anotateMethods classname ms (index+1)
  | _ -> ()

let anotateMembers prog =
  match prog with
  | Program(cs) -> List.iter (fun c -> anotateProperties c.c_properties 0; anotateMethods c.c_name.x_name c.c_methods 0) cs
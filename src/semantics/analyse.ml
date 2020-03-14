open Syntax.Tree
open Printf

let propertyOffset = -4
let argumentOffset = 16

let anotate_classes progam = 
	match progam with
	| Program(cs) -> List.iter (fun c -> printf "%s\n" c.c_name.x_name; c.c_name.x_def <- ClassDef; Hashtbl.add classTable c.c_name.x_name c) cs

let rec anotate_arguments args index = 
  match args with
  | (Prop(x, _))::props -> x.x_def <- VariableDef(argumentOffset + index); anotate_arguments props (index+4)
  | _ -> ()

let rec anotate_properties properties index =
  match properties with
  | (Prop(x, _))::props -> x.x_def <- PropertyDef(index) + index; anotateProperties props (index+4)
  | _ -> ()

let rec anotate_methods classname methods index =
  match methods with
  | m::ms -> m.m_name.x_def <- MethodDef(index); 
            if m.m_static then anotate_arguments m.m_arguments 1
            else anotate_arguments m.m_arguments 0;
            anotate_methods classname ms (index+4)
  | _ -> ()

let anotate_members prog =
  match prog with
  | Program(cs) -> List.iter 
      (fun c -> anotate_properties c.c_properties 0; 
                anotate_methods c.c_name.x_name c.c_methods 1; 
                anotate_bodies c.c_methods 0) 
      cs
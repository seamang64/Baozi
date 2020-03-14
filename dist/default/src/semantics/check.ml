open Syntax.Tree
open Syntax.Dict
open Printf

let rec check_stmt s types env = ()

let check_method c types meth = ()

let check_property types prop =
  match prop with
  | Prop(_, o) -> try ignore(Hashtbl.find types o) with 
                  Not_found ->
                    printf "Unkown type"; exit 1
  | _ -> printf "Unkown type"; exit 1

let check_class types c =
  List.iter (check_property types) c.c_properties;
  List.iter (check_method c types) c.c_methods

let check program types = 
  match program with
  | Program(cs) -> List.iter (check_class types) cs
  | _ -> printf "Unkown Program"; exit 1

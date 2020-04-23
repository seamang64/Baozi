open Syntax.Tree
open Int
open Out
open Bool
open Object
open Array
open Type

let lib_classes = ["Int", integer_def; "Output", out_def; "Bool", bool_def; "Object", object_def; "Array", array_def; "Type", type_def]

let lib_code = [object_code; integer_code; out_code; bool_code; array_code; type_code]

let start_env =
  bool_class.c_pname <- ClassType object_class;
  type_class.c_pname <- ClassType object_class;
  List.fold_right (fun (n, d) env -> define n d env) lib_classes empty
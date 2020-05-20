open Syntax.Tree
open Int
open Out
open Bool
open Object
open Array
open Type
open String

let lib_classes = ["Int", integer_def; "Output", out_def; "Bool", bool_def; "Object", object_def; "Array", array_def; "Type", type_def; "String", string_def]

let lib_method_code = [Object.method_code; Int.method_code; Bool.method_code; String.method_code; Out.method_code]

let lib_define_code = [Object.define_code; Int.define_code; Bool.define_code; Array.define_code; Type.define_code; String.define_code; Out.define_code]

and arg_x =
  {x_name="x"; x_def={d_kind=NoneKind; d_type=(ClassType bool_class)}}

let start_env =
  bool_class.c_ptype <- ClassType object_class;
  type_class.c_ptype <- ClassType object_class;
  Bool.method_get_type.m_type <- ClassType type_class;
  Bool.get_type_name.x_def.d_type <- ClassType type_class;
  Bool.method_is.m_arguments <- [Prop(arg_x, ClassType bool_class); Prop(type_arg, ClassType type_class)];
  List.fold_right (fun (n, d) env -> define n d env) lib_classes empty
open Syntax.Tree
open Syntax.Keiko
open Int
open Bool
open Object
open Type

let rec array_class =
  { c_name=array_name; c_pname=ClassType object_class; c_size=8; c_properties=[Prop(data, VoidType); Prop(length, TempType (Ident "Int"))]; c_methods=[method_equals; method_get_type; method_is; method_print]; c_ancestors=[object_class] }

and array_name =
  { x_name="Array"; x_def=array_def}

and method_equals =
  { m_name=equals_name; m_type=ClassType bool_class; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_get_type =
  { m_name=get_type_name; m_type=ClassType type_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_is =
  { m_name=is_name; m_type=ClassType bool_class; m_static=false; m_size=12; m_arguments=[Prop(arg_x, ClassType object_class); Prop(type_arg, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_print =
  { m_name=print_name; m_type=VoidType; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=ClassType bool_class}}

and get_type_name =
  {x_name="GetType"; x_def={d_kind=MethodDef (16, false); d_type=ClassType type_class}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (20, false); d_type=ClassType bool_class}}

and print_name =
  {x_name="Print"; x_def={d_kind=MethodDef (24, false); d_type=VoidType}}

and length =
  {x_name="Length"; x_def={d_kind=PropertyDef 8; d_type=(ClassType integer_class)}}

and data =
  {x_name="Data"; x_def={d_kind=PropertyDef 4; d_type=VoidType}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="x"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType array_class)}

and array_def =
  {d_kind=ClassDef; d_type=(ClassType array_class)}

let define_code =
  SEQ [
    DEFINE "Array.%desc";
    WORD (DEC 0);
    WORD (SYMBOL "Array.%anc");
    WORD (SYMBOL "Array.%string");
    WORD (SYMBOL "Object.equals");
    WORD (SYMBOL "Object.GetType");
    WORD (SYMBOL "Object.Is");
    WORD (SYMBOL "Object.Print");
    DEFINE "Array.%anc";
    WORD (DEC 2);
    WORD (SYMBOL ("Array.%desc"));
    WORD (SYMBOL ("Object.%desc"));
    DEFINE "Array.%string";
    STRING "417272617900"
  ]
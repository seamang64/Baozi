open Syntax.Tree
open Syntax.Keiko
open Int
open Bool
open Type
open Object

let rec string_class =
  { c_name=string_name; c_ptype=VoidType; c_size=4; c_properties=[Prop(length, ClassType integer_class)];
      c_methods=[method_equals; method_get_type; method_is; method_print; method_add]; c_ancestors=[]; c_generics=[] }

and string_name =
  {x_name="String"; x_def=string_def}

and string_def =
  {d_kind=ClassDef; d_type=ClassType string_class}

and method_equals =
  {m_name=equals_name; m_type=ClassType bool_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_get_type =
  { m_name=get_type_name; m_type=ClassType type_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_is =
  { m_name=is_name; m_type=ClassType bool_class; m_static=false; m_size=12; m_arguments=[Prop(arg_x, ClassType object_class); Prop(type_arg, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_print =
  { m_name=print_name; m_type=VoidType; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType string_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_add =
  {m_name=add_name; m_type=ClassType string_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, true); d_type=ClassType bool_class}}

and get_type_name =
  {x_name="GetType"; x_def={d_kind=MethodDef (16, false); d_type=ClassType type_class}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (20, false); d_type=ClassType bool_class}}

and print_name =
  {x_name="Print"; x_def={d_kind=MethodDef (24, false); d_type=ClassType string_class}}

and add_name =
  {x_name="add"; x_def={d_kind=MethodDef (28, true); d_type=ClassType string_class}}

and length =
  {x_name="Length"; x_def={d_kind=VariableDef 4; d_type=(ClassType integer_class)}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType string_class)}

and print_code =
  SEQ [
    PROC ("String.Print", 0, 0, 0);
    LOCAL 12;
    LOADW;
    GLOBAL "Output.String";
    CALLW 1;
    RETURN 0;
    END
  ]

and method_code = [print_code]

let define_code =
  SEQ [
    DEFINE "String.%desc";
    WORD (DEC 3);
    WORD (SYMBOL "String.%anc");
    WORD (SYMBOL "String.%string");
    WORD (SYMBOL "Object.equals");
    WORD (SYMBOL "Object.GetType");
    WORD (SYMBOL "Object.Is");
    WORD (SYMBOL "String.Print");
    DEFINE "String.%anc";
    WORD (DEC 2);
    WORD (SYMBOL "String.%desc");
    WORD (SYMBOL "Object.%desc");
    DEFINE "String.%string";
    STRING "537472696E6700"
  ]
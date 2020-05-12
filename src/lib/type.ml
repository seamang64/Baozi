open Syntax.Tree
open Syntax.Keiko
open Bool

let rec type_class =
  { c_name=type_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_equals]; c_ancestors=[] }

and type_name =
  { x_name="Type"; x_def=type_def}

and method_equals =
  { m_name=equals_name; m_type=ClassType bool_class; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_get_type =
  { m_name=get_type_name; m_type=ClassType type_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType type_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_is =
  { m_name=is_name; m_type=ClassType bool_class; m_static=false; m_size=12; m_arguments=[Prop(arg_x, ClassType type_class); Prop(type_arg, ClassType type_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_print =
  { m_name=print_name; m_type=VoidType; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType type_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=ClassType bool_class}}

and get_type_name =
  {x_name="GetType"; x_def={d_kind=MethodDef (16, false); d_type=ClassType type_class}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (20, false); d_type=ClassType bool_class}}

and print_name =
  {x_name="Print"; x_def={d_kind=MethodDef (24, false); d_type=VoidType}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="x"; x_def=arg_def}

and type_arg =
  {x_name="z"; x_def={d_kind=NoneKind; d_type=ClassType type_class}}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType type_class)}

and type_def =
  {d_kind=ClassDef; d_type=(ClassType type_class)}


let define_code =
  SEQ [
    DEFINE "Type.%desc";
    WORD (DEC 0);
    WORD (SYMBOL "Type.%anc");
    WORD (SYMBOL "Type.%string");
    WORD (SYMBOL "Object.equals");
    WORD (SYMBOL "Object.GetType");
    WORD (SYMBOL "Object.Is");
    WORD (SYMBOL "Object.Print");
    DEFINE "Type.%anc";
    WORD (DEC 2);
    WORD (SYMBOL ("Type.%desc"));
    WORD (SYMBOL ("Object.%desc"));
    DEFINE "Type.%string";
    STRING "5479706500"
  ]
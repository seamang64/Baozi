open Syntax.Tree
open Syntax.Keiko
open Bool
open Type

let rec object_class =
  { c_name=object_name; c_pname=VoidType; c_size=0; c_properties=[]; c_methods=[method_equals; method_get_type; method_is; method_print; method_constructor]; c_ancestors=[]; c_generics=[] }

and object_name =
  { x_name="Object"; x_def=object_def}

and method_equals =
  { m_name=equals_name; m_type=ClassType bool_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class); Prop(arg_y, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_get_type =
  { m_name=get_type_name; m_type=ClassType type_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_is =
  { m_name=is_name; m_type=ClassType bool_class; m_static=false; m_size=12; m_arguments=[Prop(arg_x, ClassType object_class); Prop(type_arg, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_print =
  { m_name=print_name; m_type=VoidType; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_constructor =
  { m_name=constructor_name; m_type=ClassType object_class; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType object_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=ClassType bool_class}}

and get_type_name =
  {x_name="GetType"; x_def={d_kind=MethodDef (16, false); d_type=ClassType type_class}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (20, false); d_type=ClassType bool_class}}

and print_name =
  {x_name="Print"; x_def={d_kind=MethodDef (24, false); d_type=VoidType}}

and constructor_name =
  {x_name="%constructor"; x_def={d_kind=MethodDef (28, false); d_type=ClassType object_class}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="x"; x_def=arg_def}

and type_arg =
  {x_name="z"; x_def={d_kind=NoneKind; d_type=ClassType type_class}}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType object_class)}

and object_def =
  {d_kind=ClassDef; d_type=(ClassType object_class)}

and equals_code =
  SEQ [
    PROC ("Object.equals", 0, 0, 0);
    LOCAL 12;
    LOADW;
    LOCAL 16;
    LOADW;
    BINOP Eq;
    GLOBAL "Bool.%desc";
    GLOBAL "baozi.makePrim";
    CALLW 2;
    RETURN 1;
    END
  ]

and get_type_code =
  SEQ [
    PROC ("Object.GetType", 0, 0, 0);
    LOCAL 12;
    LOADW;
    LOADW;
    GLOBAL "Type.%desc";
    GLOBAL "baozi.makePrim";
    CALLW 2;
    RETURN 1;
    END;
  ]

and is_code =
  let lab1 = label () and lab2 = label () and lab3 = label () in
    SEQ [
      PROC ("Object.Is", 12, 0, 0);
      LOCAL 16;
      LOADW;
      CONST 4;
      OFFSET;
      LOADW;
      LOCAL (-4);
      STOREW;
      CONST 0;
      LOCAL 12;
      LOADW;
      LOADW;
      CONST 4;
      OFFSET;
      LOADW;
      DUP 0;
      LOADW;
      LOCAL (-8);
      STOREW;
      LABEL lab1;
      SWAP;
      DUP 0;
      LOCAL (-8);
      LOADW;
      JUMPC (Eq, lab3);
      CONST 4;
      BINOP Plus;
      SWAP;
      CONST 4;
      OFFSET;
      DUP 0;
      LOADW;
      LOCAL (-4);
      LOADW;
      JUMPC (Eq, lab2);
      JUMP lab1;
      LABEL lab2;
      CONST 1;
      GLOBAL "baozi.makePrim";
      CALLW 1;
      RETURN 1;
      LABEL lab3;
      CONST 0;
      GLOBAL "baozi.makePrim";
      CALLW 1;
      RETURN 1;
      END
    ]

and print_code =
  SEQ [
    PROC ("Object.Print", 0, 0, 0);
    LOCAL 12;
    LOADW;
    LOADW;
    CONST 8;
    OFFSET;
    LOADW;
    CONST 0;
    GLOBAL "lib.print_string";
    PCALLW 1;
    RETURN 1;
    END;
  ]

let method_code = [equals_code; get_type_code; is_code; print_code]

let define_code =
  SEQ [
    DEFINE "Object.%desc";
    WORD (DEC 0);
    WORD (SYMBOL "Object.%anc");
    WORD (SYMBOL "Object.%string");
    WORD (SYMBOL "Object.equals");
    WORD (SYMBOL "Object.GetType");
    WORD (SYMBOL "Object.Is");
    WORD (SYMBOL "Object.Print");
    DEFINE "Object.%anc";
    WORD (DEC 1);
    WORD (SYMBOL "Object.%desc");
    DEFINE "Object.%string";
    STRING "4F626A65637400"
  ]
open Syntax.Tree
open Syntax.Keiko
open Bool

let rec type_class =
  { c_name=type_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_equals]; c_ancestors=[] }

and type_name =
  { x_name="Type"; x_def=type_def}

and method_equals =
  { m_name=equals_name; m_type=ClassType bool_class; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=ClassType bool_class}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="x"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType type_class)}

and type_def =
  {d_kind=ClassDef; d_type=(ClassType type_class)}

let base_equals_code =
  SEQ [
    LOCAL 12;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    LOCAL 16;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    BINOP Eq;
    GLOBAL "Bool.%desc";
    GLOBAL "baozi.makePrim";
    CALLW 2;
    RETURN 1;
    END;
  ]

let equals_code = SEQ [PROC ("Type.equals", 0, 0, 0); base_equals_code]

let define_code =
  SEQ [
    DEFINE "Type.%desc";
    WORD (DEC 0);
    WORD (DEC 1);
    WORD (SYMBOL ("Type.%anc"));
    WORD (SYMBOL "Type.equals");
    DEFINE "Type.%anc";
    WORD (DEC 2);
    WORD (SYMBOL ("Type.%desc"));
    WORD (SYMBOL ("Object.%desc"));
  ]

let type_code =
  SEQ [
    define_code;
    equals_code;
  ]
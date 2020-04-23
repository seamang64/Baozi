open Syntax.Tree
open Syntax.Keiko
open Int
open Bool
open Object

let rec array_class =
  { c_name=array_name; c_pname=ClassType object_class; c_array=false; c_size=8; c_properties=[Prop(length, TempType "Int")]; c_methods=[method_equals]; c_ancestors=[] }

and array_name =
  { x_name="Array"; x_def=array_def}

and method_equals =
  { m_name=equals_name; m_type=VoidType; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=true; m_prim_code=base_equals_code}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (8, false); d_type=ClassType bool_class}}

and length =
  {x_name="Length"; x_def={d_kind=VariableDef 4; d_type=(ClassType integer_class)}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="x"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType array_class)}

and array_def =
  {d_kind=ClassDef; d_type=(ClassType array_class)}

and base_equals_code =
  SEQ [
    CONST 8;
    GLOBAL "Bool.%desc";
    GLOBAL "lib.new";
    CALLW 2;
    DUP 0;
    GLOBAL "Bool.%desc";
    SWAP;
    STOREW;
    LOCAL (-4);
    STOREW;
    LOCAL 12;
    LOADW;
    LOCAL 16;
    LOADW;
    BINOP Eq;
    LOCAL (-4);
    LOADW;
    CONST 4;
    OFFSET;
    STOREW;
    LOCAL (-4);
    LOADW;
    RETURN 1
  ]

let equals_code = SEQ [ PROC ("Array.equals", 0, 0, 0); base_equals_code]

let define_code =
  SEQ [
    DEFINE "Array.%desc";
    WORD (DEC 0);
    WORD (SYMBOL ("Array.%anc"));
    WORD (DEC 1);
    WORD (SYMBOL "Array.equals");
    DEFINE "Array.%anc";
    WORD (SYMBOL ("Array.%desc"));
    WORD (SYMBOL ("Object.%desc"));
  ]

let array_code =
  SEQ [
    define_code;
    equals_code;
  ]
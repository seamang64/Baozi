open Syntax.Tree
open Syntax.Keiko
open Bool
open Type

let rec object_class =
  { c_name=object_name; c_pname=VoidType; c_array=false; c_size=0; c_properties=[]; c_methods=[method_equals]; c_ancestors=[] }

and object_name =
  { x_name="Object"; x_def=object_def}

and method_equals =
  { m_name=equals_name; m_type=VoidType; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_prim_code=base_equals_code}

and method_get_type =
  { m_name=get_type_name; m_type=VoidType; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_prim_code=base_get_type_code}

and method_is =
  { m_name=is_name; m_type=VoidType; m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(type_arg, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_prim_code=base_is_code}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (8, false); d_type=ClassType bool_class}}

and get_type_name =
  {x_name="getType"; x_def={d_kind=MethodDef (12, false); d_type=ClassType type_class}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (16, false); d_type=ClassType bool_class}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="x"; x_def=arg_def}

and type_arg =
  {x_name="z"; x_def={d_kind=NoneKind; d_type=ClassType type_class}}}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType object_class)}

and object_def =
  {d_kind=ClassDef; d_type=(ClassType object_class)}

let base_equals_code =
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
    RETURN 1;
    END;
  ]

let base_get_type_code =
  SEQ [
    CONST 8;
    GLOBAL "Type.%desc";
    GLOBAL "lib.new";
    CALLW 2;
    DUP 0;
    GLOBAL "Type.%desc";
    SWAP;
    STOREW;
    LOCAL (-4);
    STOREW;
    LOCAL 12;
    LOADW;
    LOADW;
    LOCAL (-4);
    LOADW;
    CONST 4;
    OFFSET;
    STOREW;
    LOCAL (-4);
    LOADW;
    RETURN 1;
    END;
  ]

let base_is_code =
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
    LOCAL 16;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    LOCAL (-8);
    STOREW;
    LOCAL 12;
    LOADW;
    CONST 8;
    OFFSET;
    LOADW:
    DUP 0;
    LOADW;
    LOCAL -12;
    STOREW;
  ]


let equals_code = SEQ [PROC ("Object.equals", 0, 0, 0); base_equals_code]
let get_type_code = SEQ [PROC ("Object.getType", 0, 0, 0); base_get_type_code]

let define_code =
  SEQ [
    DEFINE "Object.%desc";
    WORD (DEC 0);
    WORD (DEC 1);
    WORD (SYMBOL ("Object.%anc"));
    WORD (SYMBOL "Object.equals");
    DEFINE "Object.%anc";
    WORD (DEC 1);
    WORD (SYMBOL ("Object.%desc"))
  ]

let object_code =
  SEQ [
    define_code;
    equals_code;
  ]
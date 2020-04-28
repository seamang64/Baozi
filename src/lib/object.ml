open Syntax.Tree
open Syntax.Keiko
open Bool
open Type

let rec object_class =
  { c_name=object_name; c_pname=VoidType; c_array=false; c_size=0; c_properties=[]; c_methods=[method_equals; method_get_type; method_is]; c_ancestors=[] }

and object_name =
  { x_name="Object"; x_def=object_def}

and method_equals =
  { m_name=equals_name; m_type=ClassType bool_class; m_static=false; m_size=0; m_arguments=[Prop(arg_y, ClassType object_class)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and method_get_type =
  { m_name=get_type_name; m_type=ClassType type_class; m_static=false; m_size=0; m_arguments=[]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and method_is =
  { m_name=is_name; m_type=ClassType bool_class; m_static=false; m_size=12; m_arguments=[Prop(type_arg, ClassType object_class)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (8, false); d_type=ClassType bool_class}}

and get_type_name =
  {x_name="GetType"; x_def={d_kind=MethodDef (12, false); d_type=ClassType type_class}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (16, false); d_type=ClassType bool_class}}

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

and base_equals_code =
  SEQ [
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

and base_get_type_code =
  SEQ [
    LOCAL 12;
    LOADW;
    LOADW;
    GLOBAL "Bool.%desc";
    GLOBAL "baozi.makePrim";
    CALLW 2;
    RETURN 1;
    END;
  ]

and base_is_code =
  let lab1 = label () and lab2 = label () and lab3 = label () in
    SEQ [
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

let equals_code = SEQ [PROC ("Object.equals", 0, 0, 0); base_equals_code]
let get_type_code = SEQ [PROC ("Object.GetType", 0, 0, 0); base_get_type_code]
let is_code = SEQ [PROC ("Object.Is", 12, 0, 0); base_is_code]

let method_code = SEQ [equals_code; get_type_code; is_code]

let define_code =
  SEQ [
    DEFINE "Object.%desc";
    WORD (DEC 0);
    WORD (SYMBOL ("Object.%anc"));
    WORD (DEC 3);
    WORD (SYMBOL "Object.equals");
    WORD (SYMBOL "Object.GetType");
    WORD (SYMBOL "Object.Is");
    DEFINE "Object.%anc";
    WORD (DEC 1);
    WORD (SYMBOL ("Object.%desc"))
  ]

let object_code =
  SEQ [
    define_code;
    method_code;
  ]
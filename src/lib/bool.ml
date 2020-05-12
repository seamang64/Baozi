open Syntax.Tree
open Syntax.Keiko

let get_args =
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
  ]

and return_result desc =
  SEQ [
    GLOBAL desc;
    GLOBAL "baozi.makePrim";
    CALLW 2;
    RETURN 1;
  ]

let simple_operation op =
  SEQ [
    get_args;
    BINOP op;
    return_result "Bool.%desc";
    END
  ]

let base_equals_code = simple_operation Eq
let base_and_code = simple_operation And
let base_or_code = simple_operation Or

let equals_code = SEQ [PROC ("Bool.equals", 4, 0, 0); base_equals_code]
let and_code = SEQ [PROC ("Bool.and", 4, 0, 0); base_equals_code]
let or_code = SEQ [PROC ("Bool.or", 4, 0, 0); base_equals_code]

let not_code =
  SEQ [
    PROC ("Bool.not", 4, 0, 0);
    LOCAL 12;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    MONOP Not;
    return_result "Bool.%desc";
    END
  ]

let print_code =
  let lab1 = label () in
    SEQ [
      PROC ("Bool.Print", 4, 0, 0);
      LOCAL 12;
      LOADW;
      CONST 4;
      OFFSET;
      LOADW;
      CONST 0;
      JUMPC (Eq, lab1);
      GLOBAL "Bool.TrueString";
      CONST 0;
      GLOBAL "lib.print_string";
      PCALLW 1;
      RETURN 0;
      LABEL lab1;
      GLOBAL "Bool.FalseString";
      CONST 0;
      GLOBAL "lib.print_string";
      PCALLW 1;
      RETURN 0;
      END
    ]

let rec bool_class =
  { c_name=bool_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_and; method_or; method_not]; c_ancestors=[] }

and bool_name =
  { x_name="Bool"; x_def=bool_def}

and bool_def =
  {d_kind=ClassDef; d_type=(ClassType bool_class)}

and method_equals =
  { m_name=equals_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=true; m_origin=Mine}

and method_get_type =
  { m_name=get_type_name; m_type=VoidType; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType bool_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_is =
  { m_name=is_name; m_type=ClassType bool_class; m_static=false; m_size=12; m_arguments=[]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Inherited "Object"}

and method_print =
  { m_name=print_name; m_type=VoidType; m_static=false; m_size=0; m_arguments=[Prop(arg_x, ClassType bool_class)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_and =
  { m_name=and_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_or =
  { m_name=or_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and method_not =
  { m_name=not_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType)]; m_body=empty_stmt; m_main=false; m_replace=false; m_origin=Mine}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=(ClassType bool_class)}}

and get_type_name =
  {x_name="GetType"; x_def={d_kind=MethodDef (16, false); d_type=VoidType}}

and is_name =
  {x_name="Is"; x_def={d_kind=MethodDef (20, false); d_type=ClassType bool_class}}

and print_name =
  {x_name="Print"; x_def={d_kind=MethodDef (24, false); d_type=VoidType}}

and and_name =
  {x_name="and"; x_def={d_kind=MethodDef (28, false); d_type=(ClassType bool_class)}}

and or_name =
  {x_name="or"; x_def={d_kind=MethodDef (32, false); d_type=(ClassType bool_class)}}

and not_name =
  {x_name="not"; x_def={d_kind=MethodDef (36, false); d_type=(ClassType bool_class)}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType bool_class)}

and empty_stmt =
  {s_guts=Nop; s_line=(-1)}

and empty_def = {d_kind=NoneKind; d_type=VoidType}

let method_code = [equals_code; print_code; and_code; or_code; not_code]

let define_code =
  SEQ [
    DEFINE "Bool.%desc";
    WORD (DEC 0);
    WORD (SYMBOL "Bool.%anc");
    WORD (SYMBOL "Bool.%string");
    WORD (SYMBOL "Bool.equals");
    WORD (SYMBOL "Object.GetType");
    WORD (SYMBOL "Object.Is");
    WORD (SYMBOL "Bool.Print");
    WORD (SYMBOL "Bool.and");
    WORD (SYMBOL "Bool.or");
    WORD (SYMBOL "Bool.not");
    DEFINE "Bool.%anc";
    WORD (SYMBOL "Bool.%desc");
    WORD (SYMBOL "Object.%desc");
    DEFINE "Bool.%string";
    STRING "426F6F6C00";
    DEFINE "Bool.TrueString";
    STRING "5472756500";
    DEFINE "Bool.FalseString";
    STRING "46616C736500";
  ]
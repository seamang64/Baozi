open Syntax.Tree
open Syntax.Keiko


let create_bool =
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
  ]

and get_args =
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

and return_result =
  SEQ [
    LOCAL (-4);
    LOADW;
    CONST 4;
    OFFSET;
    STOREW;
    LOCAL (-4);
    LOADW;
    RETURN 1
  ]

let simple_operation op =
  SEQ [
    create_bool;
    get_args;
    BINOP op;
    return_result;
    END
  ]

let base_equals_code = simple_operation Eq
let base_and_code = simple_operation And
let base_or_code = simple_operation Or

let equals_code = SEQ [PROC ("Bool.equals", 4, 0, 0); base_equals_code]
let and_code = SEQ [PROC ("Bool.and", 4, 0, 0); base_equals_code]
let or_code = SEQ [PROC ("Bool.or", 4, 0, 0); base_equals_code]

let base_not_code =
  SEQ [
    create_bool;
    LOCAL 12;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    MONOP Not;
    return_result;
    END
  ]

let not_code = SEQ [ PROC ("Bool.not", 4, 0, 0); base_not_code]

let rec bool_class =
  { c_name=bool_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_and; method_or; method_not]; c_ancestors=[] }

and bool_name =
  { x_name="Bool"; x_def=bool_def}

and bool_def =
  {d_kind=ClassDef; d_type=(ClassType bool_class)}

and method_equals =
  { m_name=equals_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=true; m_prim_code=base_equals_code}

and method_and =
  { m_name=and_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_prim_code=base_and_code}

and method_or =
  { m_name=or_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_prim_code=base_or_code}

and method_not =
  { m_name=not_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_prim_code=base_not_code}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=(ClassType bool_class)}}

and and_name =
  {x_name="and"; x_def={d_kind=MethodDef (16, false); d_type=(ClassType bool_class)}}

and or_name =
  {x_name="or"; x_def={d_kind=MethodDef (20, false); d_type=(ClassType bool_class)}}

and not_name =
  {x_name="not"; x_def={d_kind=MethodDef (24, false); d_type=(ClassType bool_class)}}

and arg_x =
  {x_name="x"; x_def=arg_def}

and arg_y =
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType bool_class)}

and empty_def = {d_kind=NoneKind; d_type=VoidType}

let method_code = SEQ [equals_code; and_code; or_code; not_code]

let define_code =
  SEQ [
    DEFINE "Bool.%desc";
    WORD (DEC 0);
    WORD (SYMBOL "Bool.%anc");
    WORD (DEC 4);
    WORD (SYMBOL "Bool.equals");
    WORD (SYMBOL "Bool.and");
    WORD (SYMBOL "Bool.or");
    WORD (SYMBOL "Bool.not");
    DEFINE "Bool.%anc";
    WORD (SYMBOL "Bool.%desc");
    WORD (SYMBOL "Object.%desc")
  ]

let bool_code =
  SEQ [
    method_code;
    define_code
  ]
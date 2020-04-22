open Syntax.Tree
open Syntax.Keiko
open Bool

let rec integer_class =
  { c_name=integer_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=integer_methods; c_ancestors=[] }

and integer_methods = [method_add; method_times; method_sub; method_div; method_mod; method_equals; method_not_equals; method_less_than; method_greater_than; method_less_than_equal; method_greater_than_equal]

and integer_name =
  { x_name="Integer"; x_def=integer_def}

and integer_def = 
  {d_kind=ClassDef; d_type=(ClassType integer_class)}

and method_add =
  { m_name=add_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_times =
  { m_name=times_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_sub =
  { m_name=sub_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_div =
  { m_name=div_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_mod =
  { m_name=mod_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_uminus =
  { m_name=uminus_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_equals =
  { m_name=equals_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_not_equals =
  { m_name=not_equals_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_less_than =
  { m_name=less_than_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_greater_than =
  { m_name=greater_than_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_less_than_equal =
  { m_name=less_than_equal_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_greater_than_equal =
  { m_name=greater_than_equal_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and add_name =
  {x_name="add"; x_def={d_kind=MethodDef (8, false); d_type=(ClassType integer_class)}}

and times_name =
  {x_name="times"; x_def={d_kind=MethodDef (12, false); d_type=(ClassType integer_class)}}

and sub_name =
  {x_name="sub"; x_def={d_kind=MethodDef (16, false); d_type=(ClassType integer_class)}}

and div_name =
  {x_name="div"; x_def={d_kind=MethodDef (20, false); d_type=(ClassType integer_class)}}

and mod_name =
  {x_name="mod"; x_def={d_kind=MethodDef (24, false); d_type=(ClassType integer_class)}}

and uminus_name =
  {x_name="uminus"; x_def={d_kind=MethodDef (28, false); d_type=(ClassType integer_class)}}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (32, false); d_type=(ClassType bool_class)}}

and not_equals_name =
  {x_name="notEquals"; x_def={d_kind=MethodDef (36, false); d_type=(ClassType bool_class)}}

and less_than_name =
  {x_name="lessThan"; x_def={d_kind=MethodDef (40, false); d_type=(ClassType bool_class)}}

and greater_than_name =
  {x_name="greaterThan"; x_def={d_kind=MethodDef (44, false); d_type=(ClassType bool_class)}}

and less_than_equal_name =
  {x_name="lessThanEq"; x_def={d_kind=MethodDef (48, false); d_type=(ClassType bool_class)}}

and greater_than_equal_name =
  {x_name="greaterThanEq"; x_def={d_kind=MethodDef (52, false); d_type=(ClassType bool_class)}}

and arg_x =  
  {x_name="x"; x_def=arg_def}

and arg_y = 
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType integer_class)}

and empty_def = {d_kind=NoneKind; d_type=VoidType}

let create_new x = 
  SEQ [
    CONST 8;
    GLOBAL x;
    GLOBAL "lib.new";
    CALLW 2;
    DUP 0;
    GLOBAL x;
    SWAP;
    STOREW;
    LOCAL (-4);
    STOREW;
  ]

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
    LOADW
  ]

let simple_operation name t op =
  SEQ [
    PROC ("Integer." ^ name, 4, 0, 0);
    create_new t;
    get_args;
    BINOP op;
    return_result;
    END;
  ]

let add_code = simple_operation "add" "Integer.%desc" Plus
let times_code = simple_operation "times" "Integer.%desc" Times
let sub_code = simple_operation "sub" "Integer.%desc" Minus
let div_code = simple_operation "div" "Integer.%desc" Div
let mod_code = simple_operation "mod" "Integer.%desc" Mod
let equals_code = simple_operation "equals" "Bool.%desc" Eq
let not_equals_code = simple_operation "notEquals" "Bool.%desc" Neq
let less_than_code = simple_operation "lessThan" "Bool.%desc" Lt
let greater_than_code = simple_operation "greaterThan" "Bool.%desc" Gt
let less_than_equal_code = simple_operation "lessThanEq" "Bool.%desc" Leq
let greater_than_equal_code = simple_operation "greaterThanEq" "Bool.%desc" Geq

let uminus_code = 
  SEQ [
    PROC ("Integer.uminus", 4, 0, 0);
    create_new "Integer.%desc";
    LOCAL 12;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    MONOP Uminus;
    return_result;
    END;
  ]

let method_code = SEQ [add_code; times_code; sub_code; div_code; mod_code; uminus_code; equals_code; not_equals_code; less_than_code; greater_than_code; less_than_equal_code; greater_than_equal_code]

let define_code = 
  SEQ [
    DEFINE "Integer.%desc";
    WORD (DEC 0);
    WORD (DEC 12);
    WORD (SYMBOL "Integer.add");
    WORD (SYMBOL "Integer.times");
    WORD (SYMBOL "Integer.sub");
    WORD (SYMBOL "Integer.div");
    WORD (SYMBOL "Integer.mod");
    WORD (SYMBOL "Integer.uminus");
    WORD (SYMBOL "Integer.equals");
    WORD (SYMBOL "Integer.notEquals");
    WORD (SYMBOL "Integer.lessThan");
    WORD (SYMBOL "Integer.greaterThan");
    WORD (SYMBOL "Integer.lessThanEq");
    WORD (SYMBOL "Integer.greaterThanEq")
  ]

let integer_code = 
  SEQ [
    method_code;
    define_code
  ]
open Syntax.Tree
open Syntax.Keiko
open Bool

let rec integer_class =
  { c_name=integer_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_add; method_equals; method_less_than]; c_ancestors=[] }

and integer_name =
  { x_name="Integer"; x_def=integer_def}

and integer_def = 
  {d_kind=ClassDef; d_type=(ClassType integer_class)}

and method_add =
  { m_name=add_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_equals =
  { m_name=equals_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_less_than =
  { m_name=less_than_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and add_name =
  {x_name="add"; x_def={d_kind=MethodDef (8, false); d_type=(ClassType integer_class)}}

and equals_name =
  {x_name="equals"; x_def={d_kind=MethodDef (12, false); d_type=(ClassType bool_class)}}

and less_than_name =
  {x_name="lessThan"; x_def={d_kind=MethodDef (16, false); d_type=(ClassType bool_class)}}

and arg_x =  
  {x_name="x"; x_def=arg_def}

and arg_y = 
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType integer_class)}

and empty_def = {d_kind=NoneKind; d_type=VoidType}

let create_new x = 
  SEQ [
    CONST 4;
    GLOBAL x;
    GLOBAL "lib.new";
    CALLW 2;
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
    LOADW;
  ]

let add_code = 
  SEQ [
    PROC ("Integer.add", 4, 0, 0);
    create_new "Integer.%desc";
    get_args;
    BINOP Plus;
    return_result;
    END
  ]

let equals_code =
  SEQ [
    PROC ("Integer.equals", 4, 0, 0);
    create_new "Bool.%desc";
    get_args;
    BINOP Eq;
    return_result;
    END
  ]

let less_than_code =
  SEQ [
    PROC ("Integer.lessThan", 4, 0, 0);
    create_new "Bool.%desc";
    get_args;
    BINOP Lt;
    return_result;
    END
  ]

let method_code = SEQ [add_code; equals_code; less_than_code]

let define_code = 
  SEQ [
    DEFINE "Integer.%desc";
    WORD (DEC 0);
    WORD (DEC 1);
    WORD (SYMBOL "Integer.add");
    WORD (SYMBOL "Integer.equals");
    WORD (SYMBOL "Integer.lessThan")
  ]

let integer_code = 
  SEQ [
    method_code;
    define_code
  ]
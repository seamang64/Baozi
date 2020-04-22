open Syntax.Tree
open Syntax.Keiko

let rec bool_class =
  { c_name=bool_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_and; method_or; method_not]; c_ancestors=[] }

and bool_name =
  { x_name="Bool"; x_def=bool_def}

and bool_def = 
  {d_kind=ClassDef; d_type=(ClassType bool_class)}

and method_and =
  { m_name=and_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_or =
  { m_name=or_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and method_not =
  { m_name=not_name; m_type=(ClassType bool_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and and_name =
  {x_name="and"; x_def={d_kind=MethodDef (8, false); d_type=(ClassType bool_class)}}

and or_name =
  {x_name="or"; x_def={d_kind=MethodDef (12, false); d_type=(ClassType bool_class)}}

and not_name =
  {x_name="not"; x_def={d_kind=MethodDef (16, false); d_type=(ClassType bool_class)}}

and arg_x =  
  {x_name="x"; x_def=arg_def}

and arg_y =  
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType bool_class)}

and empty_def = {d_kind=NoneKind; d_type=VoidType}

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

let return_result =
  SEQ [
    LOCAL (-4);
    LOADW;
    CONST 4;
    OFFSET;
    STOREW;
    LOCAL (-4);
    LOADW;
    RETURN 1;
  ]

let and_code =
  SEQ [
    PROC ("Bool.and", 4, 0, 0);
    create_bool;
    get_args;
    BINOP And;
    return_result;
    END
  ]

let or_code =
  SEQ [
    PROC ("Bool.or", 4, 0, 0);
    create_bool;
    get_args;
    BINOP Or;
    return_result;
    END
  ]

let not_code =
  SEQ [
    PROC ("Bool.not", 4, 0, 0);
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

let method_code = SEQ [and_code; or_code; not_code]

let define_code = 
  SEQ [
    DEFINE "Bool.%desc";
    WORD (DEC 0);
    WORD (DEC 3);
    WORD (SYMBOL "Bool.and");
    WORD (SYMBOL "Bool.or");
    WORD (SYMBOL "Bool.not")
  ]

let bool_code = 
  SEQ [
    method_code;
    define_code
  ]
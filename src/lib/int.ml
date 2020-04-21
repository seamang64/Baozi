open Syntax.Tree
open Syntax.Keiko

let rec integer_class =
  { c_name=integer_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_add]; c_ancestors=[] }

and integer_name =
  { x_name="Integer"; x_def=integer_def}

and integer_def = 
  {d_kind=ClassDef; d_type=(ClassType integer_class)}

and method_add =
  { m_name=add_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and add_name =
  {x_name="add"; x_def={d_kind=MethodDef (8, false); d_type=(ClassType integer_class)}}

and arg_x =  
  {x_name="x"; x_def=arg_def}

and arg_y = 
  {x_name="y"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType integer_class)}

and empty_def = {d_kind=NoneKind; d_type=VoidType}

let method_code =
  SEQ [
    PROC ("Integer.add", 4, 0, 0);
    CONST 4;
    GLOBAL "Integer.%desc";
    GLOBAL "lib.new";
    CALLW 2;
    LOCAL (-4);
    STOREW;
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
    BINOP Plus;
    LOCAL (-4);
    LOADW;
    CONST 4;
    OFFSET;
    STOREW;
    LOCAL (-4);
    LOADW;
    RETURN 1;
    END
  ]

let define_code = 
  SEQ [
    DEFINE "Integer.%desc";
    WORD (DEC 0);
    WORD (DEC 1);
    WORD (SYMBOL "Integer.add")
    ]

let integer_code = 
  SEQ [
    method_code;
    define_code
  ]
open Syntax.Tree
open Syntax.Keiko

let rec integer_class =
  { c_name=integer_name; c_pname="Object"; c_array=false; c_size=4; c_properties=[]; c_methods=[method_add]}

and integer_name =
  { x_name="Integer"; x_def=integer_def}

and integer_def = 
  {d_kind=ClassDef; d_type=(ClassType integer_class)}

and method_add =
  { m_name=add_name; m_type=(ClassType integer_class); m_static=false; m_size=4; m_arguments=[Prop(arg_x, VoidType); Prop(arg_y, VoidType)]; m_body=Nop; m_main=false}

and add_name =
  {x_name="add"; x_def={d_kind=MethodDef (12, false); d_type=(ClassType integer_class)}}

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
    GLOBAL "NEW";
    CALLW 2;
    LOCAL (-4);
    STORE 4;
    LOCAL 16;
    LOAD 4;
    CONST 4;
    OFFSET;
    LOAD 4;
    LOCAL 20;
    LOAD 4;
    CONST 4;
    OFFSET;
    LOAD 4;
    BINOP Plus;
    LOCAL (-4);
    LOAD 4;
    CONST 4;
    OFFSET;
    STORE 4;
    LOCAL (-4);
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
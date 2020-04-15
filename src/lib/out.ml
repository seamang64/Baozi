open Syntax.Tree
open Syntax.Keiko
open Int

let rec out_class =
  { c_name=out_name; c_pname=VoidType; c_array=false; c_size=4; c_properties=[]; c_methods=[method_int]}

and out_name =
  { x_name="Output"; x_def=out_def}

and out_def = 
  {d_kind=ClassDef; d_type=(ClassType out_class)}

and method_int =
  { m_name=int_name; m_type=VoidType; m_static=true; m_size=4; m_arguments=[Prop(arg_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false}

and int_name =
  {x_name="Int"; x_def={d_kind=MethodDef (8, true); d_type=VoidType}}

and arg_x =  
  {x_name="x"; x_def=arg_def}

and arg_def =
  {d_kind=NoneKind; d_type=(ClassType integer_class)}

let method_code =
  SEQ [
    PROC ("Output.Int", 0, 0, 0);
    LOCAL 12;
    LOADW;
    CONST 4;
    OFFSET;
    LOADW;
    CONST 0;
    GLOBAL "lib.print_num";
    PCALLW 1;
    GLOBAL "lib.newline";
    PCALLW 0; 
    RETURN 0;
    END
  ]

let define_code = 
  SEQ [
    DEFINE "Output.%desc";
    WORD (DEC 0);
    WORD (DEC 1);
    WORD (SYMBOL "Output.Int")
    ]

let out_code = 
  SEQ [
    method_code;
    define_code
  ]
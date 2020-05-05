open Syntax.Tree
open Syntax.Keiko
open Int
open Object
open String

let rec out_class =
  { c_name=out_name; c_pname=ClassType object_class; c_array=false; c_size=4; c_properties=[]; c_methods=[method_int; method_intln; method_string; method_stringln; method_ln]; c_ancestors=[] }

and out_name =
  { x_name="Output"; x_def=out_def}

and out_def =
  {d_kind=ClassDef; d_type=(ClassType out_class)}

and method_int =
  { m_name=int_name; m_type=VoidType; m_static=true; m_size=0; m_arguments=[Prop(int_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and method_intln =
  { m_name=intln_name; m_type=VoidType; m_static=true; m_size=0; m_arguments=[Prop(int_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and method_string =
  { m_name=string_name; m_type=VoidType; m_static=true; m_size=0; m_arguments=[Prop(string_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and method_stringln =
  { m_name=stringln_name; m_type=VoidType; m_static=true; m_size=0; m_arguments=[Prop(string_x, VoidType)]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and method_ln =
  { m_name=ln_name; m_type=VoidType; m_static=true; m_size=0; m_arguments=[]; m_body=Nop; m_main=false; m_replace=false; m_origin=Mine}

and int_name =
  {x_name="Int"; x_def={d_kind=MethodDef (8, true); d_type=VoidType}}

and intln_name =
  {x_name="IntLn"; x_def={d_kind=MethodDef (12, true); d_type=VoidType}}

and string_name =
  {x_name="String"; x_def={d_kind=MethodDef (16, true); d_type=VoidType}}

and stringln_name =
  {x_name="StringLn"; x_def={d_kind=MethodDef (20, true); d_type=VoidType}}

and ln_name =
  {x_name="Ln"; x_def={d_kind=MethodDef (24, true); d_type=VoidType}}

and int_x =
  {x_name="x"; x_def=int_def}

and string_x =
  {x_name="x"; x_def=string_def}

and int_def =
  {d_kind=NoneKind; d_type=(ClassType integer_class)}

and string_def =
  {d_kind=NoneKind; d_type=(ClassType string_class)}

let int_code =
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
    RETURN 0;
    END
  ]

let intln_code =
  SEQ [
    PROC ("Output.IntLn", 0, 0, 0);
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

let string_code =
  SEQ [
    PROC ("Output.String", 0, 0, 0);
    LOCAL 12;
    LOADW;
    CONST 8;
    OFFSET;
    LOADW;
    CONST 0;
    GLOBAL "lib.print_string";
    PCALLW 1;
    RETURN 0;
    END
  ]

let stringln_code =
  SEQ [
    PROC ("Output.StringLn", 0, 0, 0);
    LOCAL 12;
    LOADW;
    CONST 8;
    OFFSET;
    LOADW;
    CONST 0;
    GLOBAL "lib.print_string";
    PCALLW 1;
    GLOBAL "lib.newline";
    PCALLW 0;
    RETURN 0;
    END
  ]

let ln_code =
  SEQ [
    PROC ("Output.Ln", 0, 0, 0);
    GLOBAL "lib.newline";
    PCALLW 0;
    RETURN 0;
    END
  ]

let method_code = SEQ [int_code; intln_code; string_code; stringln_code; ln_code]

let define_code =
  SEQ [
    DEFINE "Output.%desc";
    WORD (DEC 0);
    WORD (DEC 1);
    WORD (SYMBOL "Output.Int");
    WORD (SYMBOL "Output.IntLn");
    WORD (SYMBOL "Output.String");
    WORD (SYMBOL "Output.StringLn");
    WORD (SYMBOL "Output.Ln");
  ]

let out_code =
  SEQ [
    define_code;
    method_code
  ]
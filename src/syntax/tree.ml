type ident = string

type otype = string

type def = string

type name = 
  { x_name: ident; 		          
    mutable x_def: def option } (* variable,  or class *)

and expr_guts =
    Name of name
  | Parent of expr
  | Sub of expr * expr 
  | Nil
  | MethodCall of expr * name * expr list
  | Property of expr * name
  | Me

and expr = 
  { mutable e_guts: expr_guts; 
    mutable e_type: otype }

and stmt =
    Assign of expr * expr
  | Delc of name * otype * expr
  | Call of expr
  | Return of expr option
  | Seq of stmt list

and property = Prop of name * otype

and m_method =
  { m_name: name;
    m_type: otype;
    m_static: bool;
    mutable m_arugments: property list;
    mutable m_body: stmt }

and c_class =
  { c_name: name;
    c_pname: ident;
    c_array: bool;
    mutable c_properties: property list;
    mutable c_methods: m_method list }

and program =
  Program of c_class list

let createClass (n, p, arr, props, meths) = 
  { c_name=n; c_pname=p; c_array=arr; c_properties=props; c_methods=meths }

let createMethod (n, static, args, t, stmt) =
  { m_name=n; m_type=t; m_static=static; m_arugments=args; m_body=stmt }

let createExpr guts =
  { e_guts=guts; e_type="voidtype" }

let createName n =
  { x_name=n; x_def=None }
open Printf

type ident = string
and otype = string

and expr_guts =
    Name of name
  | Parent of expr
  | Sub of expr * expr 
  | Nil
  | MethodCall of expr * name * expr list
  | Property of expr * name
  | New of name
  | Me

and expr = 
  { mutable e_guts: expr_guts; 
    mutable e_type: otype }

and stmt =
    Assign of expr * expr
  | Delc of name * def_type * expr
  | Call of expr
  | Return of expr option
  | Seq of stmt list

and property = Prop of name * def_type

and m_method =
  { m_name: name;
    m_type: def_type;
    m_static: bool;
    mutable m_size: int;
    mutable m_arguments: property list;
    mutable m_body: stmt }

and c_class =
  { c_name: name;
    c_pname: ident;
    c_array: bool;
    c_size: int;
    mutable c_properties: property list;
    mutable c_methods: m_method list }

and def_kind = 
  | ClassDef
  | VariableDef of int
  | PropertyDef of int
  | MethodDef of int
  | NoneKind

and def = 
  { d_kind: def_kind;
    d_type: def_type}

and def_type =
  | ClassType of c_class
  | TempType of otype
  | VoidType

and name = 
  { x_name: ident;      
    mutable x_def: def}

and program =
  Program of c_class list

module IdMap = 
  Map.Make(struct 
    type t = ident  
    let compare = compare 
  end)

type environment = Env of def IdMap.t

let lookup id (Env(ids)) = 
  try IdMap.find id ids with
    Not_found -> printf "The name %s does not exist" id; exit 1

let add_def id classDef n = IdMap.add id classDef n

let define id d (Env(ids)) = Env(add_def id d ids)

let can f x = try f x; true with Not_found -> false

let empty = Env(IdMap.empty)

let empty_def = {d_kind=NoneKind; d_type=VoidType}

let createClass (n, p, arr, props, meths) = 
  { c_name=n; c_pname=p; c_array=arr; c_size=(List.length props) * 4; c_properties=props; c_methods=meths }

let createMethod (n, static, args, t, stmt) =
  { m_name=n; m_type=t; m_static=static; m_size=0; m_arguments=args; m_body=stmt }

let createExpr guts =
  { e_guts=guts; e_type="voidtype" }

let createName n =
  { x_name=n; x_def=empty_def }
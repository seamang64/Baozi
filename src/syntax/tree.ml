open Printf
open Errors
open Keiko

type ident = string

(* Temporary type before annotation *)
and ttype =
    Ident of ident
  | Array of ttype
  | Generic of ident * ttype list

(* Origin of method *)
and origin =
    Mine
  | Inherited of string

(* Generic type with name and most general type *)
and generic_type =
    { g_name: name;
      mutable g_ptype: def_type}

(* Language constructs *)
and expr =
    Name of name
  | Sub of expr * expr
  | Nil
  | MethodCall of expr * name * expr list
  | Property of expr * name
  | Constant of int * def_type
  | String of symbol * string
  | TypeOf of name
  | New of name
  | NewArray of name * expr
  | Parent

and stmt =
  { s_guts: stmt_guts;
    s_line: int }

and stmt_guts =
    Assign of expr * expr
  | Delc of name * def_type * expr
  | Call of expr
  | Return of expr option
  | Seq of stmt list
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt
  | ForStmt of stmt * stmt * expr * stmt
  | Nop

and property = Prop of name * def_type

and m_method =
  { m_name: name;
    mutable m_type: def_type;
    m_static: bool;
    m_main: bool;
    m_replace: bool;
    mutable m_size: int;
    mutable m_arguments: property list;
    mutable m_body: stmt;
    m_origin: origin }

and c_class =
  { c_name: name;
    mutable c_pname: def_type;
    mutable c_size: int;
    mutable c_properties: property list;
    mutable c_methods: m_method list;
    mutable c_ancestors: c_class list;
    c_generics: generic_type list }

and program =
  Program of c_class list

(* Kind of a name *)
and def_kind =
  | ClassDef
  | VariableDef of int
  | PropertyDef of int
  | MethodDef of int * bool
  | NoneKind

(* Name defintion *)
and def =
  { d_kind: def_kind;
    mutable d_type: def_type}

(* Type of name *)
and def_type =
  | ClassType of c_class
  | ArrayType of def_type
  | GenericClassType of c_class * (ident * def_type) list
  | GenericType of ident * def_type
  | TempType of ttype
  | VoidType
  | NilType

and name =
  { x_name: ident;
    mutable x_def: def}

(* Enviroment of idents to defintions *)
module IdMap =
  Map.Make(struct
    type t = ident
    let compare = compare
  end)

type environment = Env of def IdMap.t

let lookup id (Env(ids)) =
  try IdMap.find id ids with
    Not_found -> raise (UnknownName id)

let add_def id classDef n = IdMap.add id classDef n

let define id d (Env(ids)) = Env(add_def id d ids)

let can f x = try f x; true with Not_found -> false

let empty = Env(IdMap.empty)

let empty_def = {d_kind=NoneKind; d_type=VoidType}

(* create functions for parser *)
let createStmt s =
  { s_guts=s; s_line=(!Source.lineno) }

let createEmptyStmt s =
  { s_guts=s; s_line=0 }

let seq =
  function
    | [] -> createEmptyStmt Nop       (* Use Nop in place of Seq [] *)
    | [s] -> s                        (* Don't use a Seq node for one element *)
    | ss -> createEmptyStmt (Seq ss)

let createClass (n, p, props, meths, generics) =
  { c_name=n; c_pname=p; c_size=0; c_properties=props; c_methods=meths; c_ancestors=[]; c_generics=generics }

let createGeneric n t =
  { g_name=n; g_ptype=t }

let createMethod (n, static, args, t, stmt, main, replace) =
  { m_name=n; m_type=t; m_static=static; m_size=0; m_arguments=args; m_body=stmt; m_main=main; m_replace=replace; m_origin=Mine }

let createName n =
  { x_name=n; x_def=empty_def }

let createTypeName t =
  { x_name="type_delc"; x_def={d_kind=NoneKind; d_type=t} }
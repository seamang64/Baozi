open Codegen.Keiko
open Tree

type ident = string

and def_kind = 
  | ClassDef
  | VaribleDef of int
  | PropertyDef of int
  | MethodDef of int

and def = 
  { d_kind: def_kind;
    d_type: c_class}

and name = 
  { x_name: ident;      
    mutable x_def: def option }

module IdMap = 
  Map.Make(struct 
    type t = ident  
    let compare = compare 
  end)

and environment = Env of def IdMap.t

let add_def id classDef n = IdMap.add id classDef n;

let rec find_def x ns =
  match ns with
  | n:ids -> if n.x_name == x then n else search x ids
  | [] -> raise Not_Found

let let can f x = try f x; true with Not_found -> false

let lookup id Env(ids) = IdMap.find id ids (** Try-catch here? **)

let define id d Env(ids) = Env(add_def id d ids)

let empty = Env(IdMap.empty)
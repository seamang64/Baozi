open Codegen.Keiko
open Tree

type ident = string

and def = 
  | ClassDef
  | VaribleDef of int
  | PropertyDef of ident
  | MethodDef of int

and name = 
  { x_name: ident;      
    mutable x_def: def option }

module IdMap = 
  Map.Make(struct 
    type t = ident  
    let compare = compare 
  end)

and environment = Env of c_class IdMap.t

let add_def id n = IdMap.add id n;

let rec find_def x ns =
  match ns with
  | n:ids -> if n.x_name == x then n else search x ids
  | [] -> raise Not_Found

let lookup id Env(ids) = IdMap.find id ids

let empty = Env(IdMap.empty)
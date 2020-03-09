type offset = int

type ident = string

type def =
    ClassDef
  | PropertyDef of offset
  | MethodDef of offset
  | VariableDef of offset

and name = 
  { x_name: ident;              
    mutable x_def: def option }
 
type ident = string

and def = string

and name = 
  { x_name: ident;
    mutable x_offset: int;              
    mutable x_def: def option }
open Syntax.Tree
open Syntax.Keiko
open Errors
open Printf

(* Set of all arrays in the program *)
module ArraySet = Set.Make(
  struct
    let compare = compare
    type t = def_type
  end )

let arraytypes = ref ArraySet.empty

(* Generate the ancestor types of an array *)
let rec anc_types t =
  match t with
  | ClassType c | GenericClassType (c, _) -> t :: (anc_types c.c_ptype)
  | ArrayType d -> List.map (fun t' -> ArrayType t') (anc_types d)
  | VoidType -> []
  | _ -> raise IncorrectSyntaxError

(* Add and array type the set *)
let rec add_type t =
  match t with
  | ClassType _ -> ()
  | ArrayType d ->
      arraytypes := List.fold_right ArraySet.add (anc_types t) !arraytypes; (* Need to add all the ancestors *)
      add_type d (*Need to add the elements of the array *)
  | GenericType (_, _) | GenericClassType (_, _) -> raise GenericArrayError (* Cannot have arrays of generics *)
  | _ -> raise IncorrectSyntaxError

(* Make a name for an array class descriptor *)
let rec make_name t =
  match t with
  | ClassType c | GenericClassType (c, _) -> c.c_name.x_name
  | ArrayType d -> sprintf "array.%%%s" (make_name d)
  | _ -> raise IncorrectSyntaxError

(* Generate the names of the ancestor table *)
let rec gen_anc t =
  match t with
  | ClassType c | GenericClassType (c, _) -> c.c_name.x_name :: (gen_anc c.c_ptype)
  | ArrayType d -> List.map (fun s -> sprintf "array.%%%s" s) (gen_anc d)
  | GenericType (_, d) -> gen_anc d
  | VoidType -> []
  | _ -> raise IncorrectSyntaxError

(* Generate a hex string from a char *)
and gen_hex_string c =
  let chr = Char.code c and hex = "0123456789ABCDEF" in
    (Char.escaped (hex.[chr / 16])) ^ (Char.escaped (hex.[chr mod 16]))

(* Create a hex string from a string *)
and fold_string s =
  match s with
  | "" -> ""
  | _ -> (gen_hex_string (s.[0])) ^ (fold_string (String.sub s 1 ((String.length s) - 1)))

(* Generate a definintion in the data segment for a string constant *)
and gen_string (lab, s) =
  let strings = split_string s in
    let string_code = List.map (fun s -> STRING (fold_string s)) strings in
      SEQ [COMMENT (sprintf "String \"%s\"" s); DEFINE lab; SEQ string_code]

(* Split the a string into 32 bit chunks *)
and split_string s =
  let n = String.length s in
    if n > 31 then
      (String.sub s 0 32) :: (split_string (String.sub s 32 (n-32)))
    else [s ^ "\000"]

(* Generate the code for the array class descriptors *)
let create_fake_classes () =
  let gen_descriptor (ArrayType t) =
    let tn = (make_name t) and anc = gen_anc (ArrayType t) and pn = Check.print_type (ArrayType t) in
      let n = "array.%" ^ tn in
        SEQ [
          COMMENT ("Descriptor for " ^ pn);
          DEFINE (n ^ ".%desc");
          WORD (SYMBOL "0xF");
          WORD (SYMBOL (n ^ ".%anc"));
          WORD (SYMBOL (n ^ ".%string"));
          WORD (SYMBOL (tn ^ ".%desc"));
          WORD (SYMBOL "Object.equals");
          WORD (SYMBOL "Object.GetType");
          WORD (SYMBOL "Object.Is");
          WORD (SYMBOL "Object.Print");
          COMMENT ("Ancestor table for " ^ pn);
          DEFINE (n ^ ".%anc");
          WORD (DEC ((List.length anc) + 1));
          SEQ (List.map (fun s -> WORD (SYMBOL (s ^ ".%desc"))) anc);
          WORD (SYMBOL ("Object.%desc"));
          gen_string (n ^ ".%string", pn)
        ]
  in SEQ (List.map gen_descriptor (ArraySet.elements !arraytypes))
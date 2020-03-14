open Printf
open Syntax
open Semantics

let debug = ref 0

let usage = "Usage: ppc [-b] [-d n] [-O] file.p"

let spec =
  Arg.align []

let main () =
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) usage;
  (**if List.length !fns <> 1 then fprintf stderr "$\n" [fStr usage]; exit 2;**)
  let in_file = List.hd !fns in
  let in_chan = open_in in_file in
  let lexbuf = Lexing.from_channel in_chan in
  ignore (Parsing.set_trace (true));

  let prog = 
    try Parser.program Lexer.token lexbuf with
      Parsing.Parse_error -> 
        let tok = Lexing.lexeme lexbuf in printf "Syntax Error: %s\n" tok;
        exit 1 in

  (**if !debug > 0 then Tree.print_tree stdout "" prog;**)

  let types = Analyse.getClassNames prog in
  Analyse.anotateMembers prog;
  Check.check prog types;
 

  printf "MODULE Main 0 0\n";
  printf "IMPORT Lib 0\n";
  printf "ENDHDR\n\n";
(** KGEN **)
  printf "! End\n";
  exit 0;;

main ();;
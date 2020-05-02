open Printf
open Syntax
open Semantics
open Kgen

let debug = ref 0

let usage = "Usage: ppc [-b] [-d n] [-O] file.p"

let spec =
  Arg.align []

let main () =
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) usage;
  let in_file = List.hd !fns in
  let in_chan = open_in in_file in
  let lexbuf = Lexing.from_channel in_chan in
  ignore (Parsing.set_trace (true));

  let prog =
    try Parser.program Lexer.token lexbuf with
      Parsing.Parse_error ->
        let tok = Lexing.lexeme lexbuf in printf "Syntax Error: %s\n" tok;
        exit 1 in

  try
    ignore(Analyse.annotate_program prog);
    Check.check_program prog;

    printf "MODULE Main 0 0\n";
    printf "IMPORT Lib 0\n";
    printf "ENDHDR\n";

    Keiko.print_keiko(Codegen.gen_program prog);

    printf "! End\n";
    exit 0;
  with err -> Errors.print_exception err;;

main ();;
let filename = ref ""
let chan = ref stdin
let linetab = Hashtbl.create 100

let lineno = ref 1

let note_line n lexbuf =
  let line = Lexing.lexeme_end lexbuf in
  Hashtbl.add linetab n (line + 4)

let note_line_pos n =
  Hashtbl.add linetab n (pos_in !chan)

let get_line n =
  let pos0 = pos_in !chan in
  let line =
    try seek_in !chan (Hashtbl.find linetab n); input_line !chan with
        Not_found -> ""
      | End_of_file -> "" in
  seek_in !chan pos0;
  line

let init fn ch =
  filename := fn; chan := ch;
  Hashtbl.add linetab 1 0
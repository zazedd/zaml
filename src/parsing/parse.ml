type c = { content : string; filename : string }

let from_string str : c = { content = str; filename = "" }

let from_file file =
  try
    let ic = open_in file in
    let len = in_channel_length ic in
    let str = really_input_string ic len in
    close_in ic;
    { content = str; filename = file }
  with
  | Sys_error e ->
      print_endline ("Error: " ^ e);
      exit 1
  | e -> raise e

let parse s =
  let lexbuf = Lexing.from_string s.content in
  Lexing.set_filename lexbuf s.filename;
  try Parser.prog Lexer.read lexbuf
  with Parser.Error -> Errors.parsing_error lexbuf

open Ast.Common
open Ast.Parsed

exception LexingError of string
exception ParsingError of string

let which_chars start ending =
  if start = ending then Format.sprintf "Character %d" start
  else Format.sprintf "Characters %d-%d" start ending

let error_pos pos =
  match pos.fname with
  | "" -> "Syntax Error: " ^ which_chars pos.start pos.ending
  | fname ->
      Format.sprintf "Syntax Error: File %s on Line %d ->\n\t      %s" fname
        pos.lnum
        (which_chars pos.start pos.ending)

let lexing_error (lexbuf : Lexing.lexbuf) c =
  let pos =
    position
      ( (lexbuf.lex_curr_p, lexbuf.lex_curr_p),
        (lexbuf.lex_curr_p, lexbuf.lex_curr_p) )
  in
  let s = Format.sprintf "%s: Unknown character: '%c'" (error_pos pos) c in
  raise (LexingError s)

let parsing_error (lexbuf : Lexing.lexbuf) =
  let pos =
    position
      ( (lexbuf.lex_start_p, lexbuf.lex_start_p),
        (lexbuf.lex_curr_p, lexbuf.lex_curr_p) )
  in
  let s = Format.sprintf "%s." (error_pos pos) in
  raise (ParsingError s)

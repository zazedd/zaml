{
  open Parser
  open Errors

  let new_line f (lexbuf : Lexing.lexbuf) =
    Lexing.new_line lexbuf;
    let newpos = { lexbuf.lex_curr_p with pos_bol = 0; pos_cnum = 0 } in
    lexbuf.lex_curr_p <- newpos;
    lexbuf.lex_start_p <- newpos;
    f lexbuf

  let make_string str = String.sub str 1 (String.length str - 2)
}

let white = [' ' '\t']+
let newl = '\n'
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['_' 'a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let letter_space = ['_' 'a'-'z' ' ' 'A'-'Z'] ['a'-'z' 'A'-'Z' ' ' '_' '0'-'9']*
let char = '\'' ['a'-'z'] '\''
let str = '\"' letter_space+ '\"'
let ident = letter+

rule read =
  parse
  | white { read lexbuf }
  | newl { new_line read lexbuf }
  | ";" { SEMICOLON }
  | "++" { PLUSPLUS }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | "%" { MODULUS }
  | "::" { CONS }
  | "=" { EQUALS }
  | "==" { EQ }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { BT }
  | ">=" { BTE }
  | "!=" { INEQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LSQBRACKET }
  | "]" { RSQBRACKET }
  | "true" { TRUE }
  | "false" { FALSE }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "->" { ARROW }
  | ":" { COLON }
  | "int" { TINT }
  | "bool" { TBOOL }
  | "()" { UNIT }
  | "match" { MATCH }
  | "with" { WITH }
  | "|" { BAR }
  | ".." { DOTDOT }
  | char { CHAR ( Lexing.lexeme_char lexbuf 1 )}
  | str { STR ( Lexing.lexeme lexbuf |> make_string )}
  | ident { IDENT ( Lexing.lexeme lexbuf ) }
  | int { INT ( Lexing.lexeme lexbuf |> int_of_string ) }
  | eof { EOF }
  | _ as c { lexing_error lexbuf c }


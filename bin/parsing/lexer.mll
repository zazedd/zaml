{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let ident = letter+

rule read =
  parse
  | white { read lexbuf }
  | "+" { PLUS }
  | "*" { MULT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "let" { LET }
  | "=" { EQUALS }
  | "==" { EQ }
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
  | ident { IDENT ( Lexing.lexeme lexbuf ) }
  | int { INT ( Lexing.lexeme lexbuf |> int_of_string ) }
  | eof { EOF }


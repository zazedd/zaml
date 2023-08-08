%{
  open Ast.Parsed

  let rec make_apply acc (pos : Ast.Common.pos) e = function
    | [] -> failwith "precondition violated"
    | [e'] -> { expr = App (e, (e' :: acc) |> List.rev); pos }
	  | h :: ((_ :: _) as t) -> make_apply (h :: acc) pos e t
%}

%token <int> INT
%token <string> IDENT

%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token ARROW
%token MATCH
%token WITH
%token EOF

%token SEMICOLON
%token LPAREN
%token RPAREN
%token EQUALS
%token COLON
%token DOTDOT
%token BAR

%token PLUS
%token MULT
%token EQ

%token TRUE
%token FALSE
%token TINT
%token TBOOL
%token UNIT

%nonassoc IN
%nonassoc ELSE
%left EQ
%left PLUS
%left MULT

%start <Ast.Parsed.expr list> prog

%%

prog:
  | e = expr+; EOF { e }
  ;

expr:
  | e = simple_expr { e }
  | e1 = expr; PLUS; e2 = expr { { expr = Bop (Add, e1, e2); pos = position ($loc, $loc(e2)) } }
  | e1 = expr; MULT; e2 = expr { { expr = Bop (Mult, e1, e2); pos = position ($loc, $loc(e2)) } }
  | e1 = expr; EQ; e2 = expr { { expr = Bop (Eq, e1, e2); pos = position ($loc, $loc(e2)) } }
  | LET; name = IDENT; vars = list(IDENT); EQUALS; b = expr; SEMICOLON? {
    {
      expr = Let { name; binding = { expr = Lambda { vars; body = b }; pos = position ($loc, $loc(b)) }; in_body = None }; pos = position ($loc, $loc(b))
    }
  }
  | LET; name = IDENT; EQUALS; binding = expr; SEMICOLON? {
    {
      expr = Let { name; binding; in_body = None }; pos = position ($loc, $loc(binding))
    }
  }
  | LET; name = IDENT; vars = list(IDENT); EQUALS; b = expr; IN; body = expr; SEMICOLON? {
    {
      expr = Let { name; binding = { expr = Lambda { vars; body = b }; pos = position ($loc, $loc(b)) }; in_body = Some body }; pos = position ($loc, $loc($8))
    }
  }
  | LET; name = IDENT; EQUALS; binding = expr; IN; body = expr; SEMICOLON? {
    {
      expr = Let { name; binding; in_body = Some body }; pos = position ($loc, $loc($7))
    }
  }
  | IF; b = expr ; THEN; e1 = expr; ELSE; e2 = expr; SEMICOLON? {
    {
      expr = If (b, e1, e2); pos = position ($loc, $loc($7))
    }
  }
  | FUN; vars = list(IDENT); ARROW; body = expr; SEMICOLON?; {
    {
      expr = Lambda { vars; body }; pos = position ($loc, $loc(body))
    }
  }
  | e = simple_expr; es = simple_expr+; SEMICOLON? { make_apply [] (position ($loc, $loc(es))) e es }
  ;

simple_expr:
  | i = INT { { expr = Int i; pos = position ($loc, $loc(i)) } }
  | x = IDENT { { expr = Var x; pos = position ($loc, $loc(x)) } }
  | UNIT { { expr = Unit; pos = position ($loc, $loc($1)) } }
  | TRUE { { expr = Bool true; pos = position ($loc, $loc($1)) } }
  | FALSE { { expr = Bool false; pos = position ($loc, $loc($1)) } }
  | LPAREN; e = expr; RPAREN { e }
;

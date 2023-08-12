%{
  open Ast.Parsed
  open Evaluating.Errors

  let rec make_apply acc (pos : Ast.Common.pos) e = function
    | [] -> failwith "precondition violated"
    | [e'] -> { expr = App (e, (e' :: acc) |> List.rev); pos }
	  | h :: ((_ :: _) as t) -> make_apply (h :: acc) pos e t

  let make_list = function
    | None -> List []
    | Some lst -> List lst

  let rec make_range i1 i2 acc pos =
    if i1 > i2 then range_error ()
    else if i1 = i2 then List ( { expr = Const (Int i1); pos = pos } :: acc)
    else make_range i1 (i2 - 1) ( { expr = Const (Int i2); pos = pos } :: acc) pos
%}

%token <int> INT
%token <char> CHAR
%token <string> STR
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
%token LSQBRACKET
%token RSQBRACKET
%token EQUALS
%token COLON
%token DOTDOT
%token BAR

%token PLUSPLUS
%token PLUS
%token MINUS
%token MULT
%token DIV
%token MODULUS
%token AT
%token CONS
%token EQ
%token INEQ
%token LT
%token LTE
%token BT
%token BTE

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
  | e = expr_semicolon+; EOF { e }
  ;

expr_semicolon:
  | e = expr; SEMICOLON?; { e }
  ;

expr:
  | e = const { e }
  | e1 = expr; op = bop; e2 = expr { { expr = Bop (op, e1, e2); pos = position ($loc, $loc(e2)) } }
  | LET; name = IDENT; vars = list(IDENT); EQUALS; b = expr; {
    {
      expr = Let { name; binding = { expr = Lambda { vars; body = b }; pos = position ($loc, $loc(b)) }; in_body = None }; pos = position ($loc, $loc(b))
    }
  }
  | LET; name = IDENT; EQUALS; binding = expr; {
    {
      expr = Let { name; binding; in_body = None }; pos = position ($loc, $loc(binding))
    }
  }
  | LET; name = IDENT; vars = list(IDENT); EQUALS; b = expr; IN; body = expr; {
    {
      expr = Let { name; binding = { expr = Lambda { vars; body = b }; pos = position ($loc, $loc(b)) }; in_body = Some body }; pos = position ($loc, $loc(body))
    }
  }
  | LET; name = IDENT; EQUALS; binding = expr; IN; body = expr; {
    {
      expr = Let { name; binding; in_body = Some body }; pos = position ($loc, $loc(body))
    }
  }
  | IF; b = expr ; THEN; e1 = expr; ELSE; e2 = expr; {
    {
      expr = If (b, e1, e2); pos = position ($loc, $loc(e2))
    }
  }
  | FUN; vars = list(IDENT); ARROW; body = expr; {
    {
      expr = Lambda { vars; body }; pos = position ($loc, $loc(body))
    }
  }
  | e = const; es = const+; { make_apply [] (position ($loc, $loc(es))) e es }
  ;

bop:
  | PLUS { Add }
  | PLUSPLUS { Concat }
  | MINUS { Subt }
  | MULT { Mult }
  | DIV { Div }
  | MODULUS { Mod }
  | AT { LstAppend }
  | CONS { Cons }
  | EQ { Eq }
  | INEQ { Ineq }
  | LT { Lt }
  | LTE { Lte }
  | BT { Bt }
  | BTE { Bte }
  ;

const:
  | e = simple_expr { { expr = Const e; pos = position ($loc, $loc(e)) } }
  | x = IDENT { { expr = Var x; pos = position ($loc, $loc(x)) } }
  | LSQBRACKET; lst = expr_list?; RSQBRACKET { { expr = Const (make_list lst); pos = position ($loc, $loc($3)) } }
  | i1 = INT; DOTDOT; i2 = INT { { expr = Const (make_range i1 i2 [] (position ($loc, $loc(i2)))); pos = position ($loc, $loc(i2)) } }
  | LPAREN; e = expr; RPAREN { e }
  ;

expr_list:
  | e = expr { [e] }
  | e = expr; SEMICOLON; lst = expr_list { e :: lst }
  ;

simple_expr:
  | i = INT { Int i }
  | c = CHAR { Char c }
  | s = STR { String s }
  | UNIT { Unit }
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

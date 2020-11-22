%token INDENT DEDENT NEWLINE

%token L_PAREN "("
%token R_PAREN ")"
%token L_BRACKET "{"
%token R_BRACKET "}"
%token COMMA ","
%token COLON ":"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token SLASH "/"

%token UNIT
%token <bool> BOOL
%token <Int64.t> INT
%token <float> FLOAT
%token <string> CHAR
%token <string> STRING

%token <string> IDENT

%token EOF

%left "+" "-"
%left "*" "/"

%{
  open Ast
%}

%start <expr> prog
%%

prog:
  | e = expr; NEWLINE; EOF { e }
expr:
  | UNIT { Literal Unit }
  | x = BOOL { Literal (Bool x) }
  | x = INT { Literal (Int x) }
  | x = FLOAT { Literal (Float x) }
  | x = CHAR { Literal (Char x) }
  | x = STRING { Literal (String x) }
  | id = IDENT { Ident id }
  | "("; e = expr; ")" { e }
  | left = expr; "+"; right = expr { BinOp { op=Add; left; right } }
  | left = expr; "-"; right = expr { BinOp { op=Sub; left; right } }
  | left = expr; "*"; right = expr { BinOp { op=Mul; left; right } }
  | left = expr; "/"; right = expr { BinOp { op=Div; left; right } }

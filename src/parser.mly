%token L_PAREN "("
%token R_PAREN ")"
%token L_BRACKET "{"
%token R_BRACKET "}"
%token L_ANGLE_BRACKET "<"
%token R_ANGLE_BRACKET ">"
%token COMMA ","
%token COLON ":"
%token EQ "="
%token BANG_EQ "!="
%token LT_EQ "<="
%token GT_EQ ">="
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token SLASH "/"
%token ARROW "=>"

%token LET "let"
%token IF "if"
%token THEN "then"
%token ELSE "else"

%token <bool> BOOL
%token <Int64.t> INT
%token <float> FLOAT
%token <string> CHAR
%token <string> STRING

%token <string> IDENT

%token EOF

%token IN

%nonassoc IN
%left "=" "!="
%left "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/"

%{
  open Ast
%}

%start <expr> prog
%%

prog:
  | e = expr; EOF { e }

expr:
  | e = primary { e }
  | "("; e = expr; ")" { e }
  | left = expr; op = binop; right = expr { BinOp { op; left; right } }
  | "let"; name = IDENT; EQ; value = expr; IN; result = expr { Let { name; value; result } }

%inline primary:
  | "("; ")" { Literal Unit }
  | x = BOOL { Literal (Bool x) }
  | x = INT { Literal (Int x) }
  | x = FLOAT { Literal (Float x) }
  | x = CHAR { Literal (Char x) }
  | x = STRING { Literal (String x) }
  | id = IDENT { Ident id }

%inline binop:
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }
  | "=" { Eq }
  | "!=" { NotEq }
  | "<" { Lt }
  | ">" { Gt }
  | "<=" { LtEq }
  | ">=" { GtEq }

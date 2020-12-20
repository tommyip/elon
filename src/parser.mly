%token INDENT DEDENT NEWLINE

%token L_PAREN "("
%token R_PAREN ")"
%token L_BRACKET "{"
%token R_BRACKET "}"
%token COMMA ","
%token COLON ":"
%token EQ "="
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token SLASH "/"

%token LET

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
  | e = compound_expr; NEWLINE; EOF { e }
compound_expr:
  | LET; name = IDENT; EQ; value = expr; NEWLINE; result = compound_expr { Let { name; value; result } }
  | e = expr { e }
expr:
  | UNIT { Literal Unit }
  | x = BOOL { Literal (Bool x) }
  | x = INT { Literal (Int x) }
  | x = FLOAT { Literal (Float x) }
  | x = CHAR { Literal (Char x) }
  | x = STRING { Literal (String x) }
  | id = IDENT { Ident id }
  | "("; e = expr; ")" { e }
  | left = expr; op = binop; right = expr { BinOp { op; left; right } }
%inline binop:
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }

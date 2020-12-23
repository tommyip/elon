%token INDENT DEDENT NEWLINE

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

%token UNIT
%token <bool> BOOL
%token <Int64.t> INT
%token <float> FLOAT
%token <string> CHAR
%token <string> STRING

%token <string> IDENT

%token EOF

%nonassoc "else"
%nonassoc "=>"
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
  | e = expr; NEWLINE?; EOF { e }

expr:
  | e = multiline_expr { e }
  | e = inline_expr { e }

multiline_expr:
  | e = let_expr { e }
  | e = conditional_expr { e }

inline_expr:
  | e = inline_expr_not_id { e }
  | id = IDENT { Ident id }

inline_expr_not_id:
  | e = simple_expr { e }
  | e = inline_conditional_expr { e }
  | e = inline_function_expr { e }

block:
  | NEWLINE; INDENT; e = expr; NEWLINE?; DEDENT { e }

let_expr:
  | "let"; name = IDENT; EQ; value = inline_expr; NEWLINE;
    result = expr  { Let { name; value; result } }
  | "let"; name = IDENT; EQ;
      value = block;
    result = expr { Let { name; value; result } }

conditional_expr:
  | "if"; cond = inline_expr; "then";
      consequent = block;
    "else";
      alternative = block; { Conditional { cond; consequent; alternative } }

%inline inline_conditional_expr:
  | "if"; cond = inline_expr; "then"; consequent = inline_expr;
    "else"; alternative = inline_expr { Conditional { cond; consequent; alternative } }

parameter_list:
  | UNIT { [] }
  | "("; param = IDENT; ")" { [param] }
  | "("; hd = IDENT; ","; tl = separated_nonempty_list(",", IDENT); ")" { hd :: tl }

%inline inline_function_expr:
  | params = parameter_list; "=>"; body = inline_expr { Function { params; body } }

%inline simple_expr:
  | UNIT { Literal Unit }
  | x = BOOL { Literal (Bool x) }
  | x = INT { Literal (Int x) }
  | x = FLOAT { Literal (Float x) }
  | x = CHAR { Literal (Char x) }
  | x = STRING { Literal (String x) }
  | "("; e = inline_expr_not_id; ")" { e }
  | "("; id = IDENT; ")" { Ident id }
  | left = inline_expr; op = binop; right = inline_expr { BinOp { op; left; right } }

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

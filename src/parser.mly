%token L_PAREN "("
%token R_PAREN ")"
%token L_BRACE "{"
%token R_BRACE "}"
%token L_BRACKET "["
%token R_BRACKET "]"
%token L_CHEVRON "<"
%token R_CHEVRON ">"
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
%token ARROW "->"
%token FAT_ARROW "=>"

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

%nonassoc IN, ELSE, "=>"
%left "=" "!="
%left "<" ">" "<=" ">="
%left "+" "-"
%left "*" "/"
%nonassoc "("

%{
  open Ast
  open Helpers

  let src = Logs.Src.create "parser"
  module Log = (val Logs.src_log src : Logs.LOG)
%}

%start <expr> prog
%%

prog: e = expr; EOF {
  Log.debug (fun m -> m "%a" pp_expr e);
  e
}

expr:
  | e = expr_not_id { e }
  | id = IDENT { Ident id }

expr_not_id:
  | e = primary { e }
  | e = paranthesize_expr { e }
  | left = expr; op = binop; right = expr { BinOp { op; left; right } }
  | "let"; name = IDENT; typing = let_typing?; EQ; value = expr; IN; result = expr
    { Let { name; typing; value; result } }
  | "if"; cond = expr; "then"; consequent = expr; "else"; alternative = expr
    { Conditional { cond; consequent; alternative } }
  | params = parameter_list; "=>"; body = expr { Lambda (Untyped { params; body }) }
  | e = typed_lambda_expr { e }
  | fn = expr; "("; args = separated_list(",", expr); ")" { FnApplication { fn; args } }
  | "["; lst = separated_list(",", expr); "]" { List lst }

let_typing: ":" typing = typing { typing }
typing:
  | name = IDENT { { name; params=[] } }
  | name = IDENT; "<"; params = separated_nonempty_list(",", typing); ">" { { name; params } }

parameter_list:
  | "("; ")" { [] }
  | "("; param = IDENT; ")" { [param] }
  | "("; hd = IDENT; ","; tl = separated_nonempty_list(",", IDENT); ")" { hd :: tl }

%inline typed_lambda_expr:
  | typing = typed_parameter_list; "=>"; body = expr
    { let params, return = typing in Lambda (Typed { params; return; body }) }

typed_parameter_list:
  | "("; ")"; "->"; return = typing { ([], return) }
  | "("; params = separated_nonempty_list(",", separated_pair(IDENT, ":", typing)); ")";
    "->"; return = typing { (params, return) }

%inline paranthesize_expr:
  | "("; e = expr_not_id; ")" { e }
  | "("; id = IDENT; ")" { Ident id }

%inline primary:
  | "("; ")" { Literal Unit }
  | x = BOOL { Literal (Bool x) }
  | x = INT { Literal (Int x) }
  | x = FLOAT { Literal (Float x) }
  | x = CHAR { Literal (Char x) }
  | x = STRING { Literal (String x) }

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

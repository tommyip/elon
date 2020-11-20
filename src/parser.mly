%token <Int64.t> INT
%token PLUS
%token MINUS
%token TIMES
%token SLASH
%token EOF

%{
  open Ast
%}

%start <expr> prog
%%

prog:
  | e=expr EOF { e }
  ;
expr:
  | e=additive_expr { e }
  ;
additive_expr:
  | left=additive_expr PLUS right=multiplicative_expr { BinOp { op=Add; left; right } }
  | left=additive_expr MINUS right=multiplicative_expr { BinOp { op=Sub; left; right } }
  | e=multiplicative_expr { e }
  ;
multiplicative_expr:
  | left=multiplicative_expr TIMES right=atom { BinOp { op=Mul; left; right } }
  | left=multiplicative_expr SLASH right=atom { BinOp { op=Div; left; right } }
  | e=atom { e }
  ;
atom:
  | x = INT { Literal (Int x) }
  ;

type t
  = L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_ANGLE_BRACKET
  | R_ANGLE_BRACKET
  | COMMA
  | COLON
  | EQ
  | BANG_EQ
  | LT_EQ
  | GT_EQ
  | PLUS
  | MINUS
  | TIMES
  | SLASH
  | ARROW

  | LET
  | IF
  | THEN
  | ELSE

  | BOOL of bool [@printer fun fmt -> fprintf fmt "BOOL %b"]
  | INT of Int64.t [@printer fun fmt -> fprintf fmt "INT %Ld"]
  | FLOAT of float [@printer fun fmt -> fprintf fmt "FLOAT %f"]
  | CHAR of string [@printer fun fmt -> fprintf fmt "CHAR '%s'"]
  | STRING of string [@printer fun fmt -> fprintf fmt "STRING \"%s\""]

  | IDENT of string [@printer fun fmt -> fprintf fmt "IDENT %s"]
  | EOF

  | IN
  [@@deriving show { with_path = false }]

(* Menhir looks for [Module.token] type *)
type token = t

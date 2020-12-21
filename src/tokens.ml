type t
  = INDENT
  | DEDENT
  | NEWLINE

  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | COMMA
  | COLON
  | EQ
  | PLUS
  | MINUS
  | TIMES
  | SLASH

  | LET

  | UNIT
  | BOOL of bool [@printer fun fmt -> fprintf fmt "BOOL %b"]
  | INT of Int64.t [@printer fun fmt -> fprintf fmt "INT %Ld"]
  | FLOAT of float [@printer fun fmt -> fprintf fmt "FLOAT %f"]
  | CHAR of string [@printer fun fmt -> fprintf fmt "CHAR '%s'"]
  | STRING of string [@printer fun fmt -> fprintf fmt "STRING \"%s\""]

  | IDENT of string [@printer fun fmt -> fprintf fmt "IDENT %s"]
  | EOF
  [@@deriving show { with_path = false }]

(* Menhir looks for [Module.token] type *)
type token = t

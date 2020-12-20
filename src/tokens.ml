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
  | BOOL of bool
  | INT of Int64.t [@printer fun fmt -> fprintf fmt "(INT %Ld)"]
  | FLOAT of float
  | CHAR of string
  | STRING of string

  | IDENT of string
  | EOF
  [@@deriving show { with_path = false }]

(* Menhir looks for [Module.token] type *)
type token = t

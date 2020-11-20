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
  | PLUS
  | MINUS
  | TIMES
  | SLASH

  | UNIT
  | BOOL of bool
  | INT of Int64.t
  | FLOAT of float
  | CHAR of string
  | STRING of string

  | IDENT of string
  | EOF
  [@@deriving show { with_path = false }]

(* Menhir looks for [Module.token] type *)
type token = t

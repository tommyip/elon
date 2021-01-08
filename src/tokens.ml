type t
  = L_PAREN
  | R_PAREN
  | L_BRACE
  | R_BRACE
  | L_BRACKET
  | R_BRACKET
  | L_CHEVRON
  | R_CHEVRON
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
  | FAT_ARROW

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

  (* Artificial tokens to be injected by the preparser *)
  | IN
  [@@deriving show { with_path = false }, eq]

(* Menhir looks for [Module.token] type *)
type token = t

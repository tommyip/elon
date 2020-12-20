type bin_op
  = Add
  | Sub
  | Mul
  | Div
  [@@deriving show { with_path = false }]

type literal
  = Unit
  | Bool of bool
  | Int of Int64.t [@printer fun fmt -> fprintf fmt "(Int %Ld)"]
  | Float of float
  | Char of string
  | String of string

type expr
  = Let of { name: string; value: expr; result: expr }
  | BinOp of { op: bin_op; left: expr; right: expr }
  | Literal of literal
  | Ident of string

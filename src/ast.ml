open CCFormat

type bin_op
  = Add
  | Sub
  | Mul
  | Div

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

let pp_bin_op fmt = function
  | Add -> pp_print_char fmt '+'
  | Sub -> pp_print_char fmt '-'
  | Mul -> pp_print_char fmt '*'
  | Div -> pp_print_char fmt '/'

let pp_literal fmt = function
  | Unit -> pp_print_string fmt "()"
  | Bool b -> pp_print_bool fmt b
  | Int x -> pp_print_int fmt (Int64.to_int x)
  | Float f -> pp_print_float fmt f
  | Char c -> fprintf fmt "'%s'" c
  | String s -> fprintf fmt "\"%s\"" s

let rec pp_expr fmt = function
  | Let { name; value; result } ->
    fprintf fmt "@[<hov 2>(let %s@;<1>%a@;<1>%a@,)@]" name pp_expr value pp_expr result
  | BinOp { op; left; right } ->
    fprintf fmt "@[<hov 2>(%a@;<1>%a@;<1>%a@,)@]" pp_bin_op op pp_expr left pp_expr right
  | Literal lit -> pp_literal fmt lit
  | Ident id -> pp_print_string fmt id

type bin_op
  = Add
  | Sub
  | Mul
  | Div

type literal
  = Int of Int64.t

type expr
  = BinOp of { op: bin_op; left: expr; right: expr }
  | Literal of literal

let sexp_str lst =
  let content = String.concat " " lst in
  "(" ^ content ^ ")"

let bin_op_str = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"

let literal_str = function
  | Int x -> Int64.to_string x

let rec expr_str = function
  | BinOp { op; left; right } -> sexp_str [bin_op_str op; expr_str left; expr_str right]
  | Literal lit -> literal_str lit

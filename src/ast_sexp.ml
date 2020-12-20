open Ast
open CCSexp

let literal_sexp = function
  | Unit -> of_unit
  | Bool b -> of_pair (atom "Bool", of_bool b)
  | Int i -> of_pair (atom "Int", atom (Int64.to_string i))
  | Float x -> of_pair (atom "Float", of_float x)
  | Char c -> of_pair (atom "Char", atom c)
  | String s -> of_pair (atom "String", atom s)

let rec expr_sexp = function
  | Let { name; value; result } -> of_triple
      (atom "let", of_pair (atom name, expr_sexp value), expr_sexp result)
  | BinOp { op; left; right } -> CCSexp.of_triple
      (atom (show_bin_op op), expr_sexp left, expr_sexp right)
  | Literal lit -> literal_sexp lit
  | Ident id -> CCSexp.atom id

let pp fmt ast = CCSexp.pp fmt (expr_sexp ast)

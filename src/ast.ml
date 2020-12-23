open CCFormat

type bin_op
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | NotEq
  | Lt
  | Gt
  | LtEq
  | GtEq

type literal
  = Unit
  | Bool of bool
  | Int of Int64.t [@printer fun fmt -> fprintf fmt "(Int %Ld)"]
  | Float of float
  | Char of string
  | String of string

type expr
  = Let of { name: string; value: expr; result: expr }
  | Conditional of { cond: expr; consequent: expr; alternative: expr }
  | Function of { params: string list; body: expr }
  | BinOp of { op: bin_op; left: expr; right: expr }
  | Literal of literal
  | Ident of string

let pp_bin_op fmt bin_op =
  let sym = match bin_op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "="
    | NotEq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | LtEq -> "<="
    | GtEq -> ">="
  in pp_print_string fmt sym

let pp_literal fmt = function
  | Unit -> pp_print_string fmt "()"
  | Bool b -> pp_print_bool fmt b
  | Int x -> pp_print_int fmt (Int64.to_int x)
  | Float f -> pp_print_float fmt f
  | Char c -> fprintf fmt "'%s'" c
  | String s -> fprintf fmt "\"%s\"" s

let pp_string_list fmt lst =
  pp_print_char fmt '(';
  List.iteri (fun i s ->
    if i > 0 then
      pp_print_break fmt 1 0;
    pp_print_string fmt s) lst;
  pp_print_char fmt ')'

let rec pp_expr fmt = function
  | Let { name; value; result } ->
    fprintf fmt "@[<hov 2>(let %s@;<1>%a@;<1>%a@,)@]" name pp_expr value pp_expr result
  | Conditional { cond; consequent; alternative } ->
    fprintf fmt "@[<hov 2>(if %a@;<1>@[<hv>%a@;<1>%a@]@,)@]"
      pp_expr cond pp_expr consequent pp_expr alternative
  | Function { params; body } ->
    fprintf fmt "@[<hov 2>(λ %a@;<1>%a@,)@]" pp_string_list params pp_expr body
  | BinOp { op; left; right } ->
    fprintf fmt "@[<hov 2>(%a@;<1>%a@;<1>%a@,)@]" pp_bin_op op pp_expr left pp_expr right
  | Literal lit -> pp_literal fmt lit
  | Ident id -> pp_print_string fmt id

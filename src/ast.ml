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

type typing =
  { name: string;
    params: typing list;
  }

type expr
  = Let of { name: string; typing: typing option; value: expr; result: expr }
  | Conditional of { cond: expr; consequent: expr; alternative: expr }
  | Lambda of { params: string list; body: expr }
  | FnApplication of { fn: expr; args: expr list }
  | BinOp of { op: bin_op; left: expr; right: expr }
  | Literal of literal
  | Ident of string
  | List of expr list

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

let pp_list ~l ~r fmt pp lst =
    pp_open_hovbox fmt 1;
    pp_print_char fmt l;
    CCList.pp ~pp_sep:(fun fmt () -> pp_print_char fmt ','; pp_print_break fmt 1 0) pp fmt lst;
    pp_print_cut fmt ();
    pp_print_char fmt r;
    pp_close_box fmt ()

let rec pp_typing fmt { name; params } =
  pp_print_string fmt name;
  if List.length params > 0 then
    pp_list ~l:'<' ~r:'>' fmt pp_typing params

let pp_string_list fmt lst =
  pp_print_char fmt '(';
  List.iteri (fun i s ->
    if i > 0 then
      pp_print_break fmt 1 0;
    pp_print_string fmt s) lst;
  pp_print_char fmt ')'

let rec pp_args_list fmt lst =
  if List.length lst > 0 then
    pp_print_break fmt 1 0;
    pp_open_hvbox fmt 0;
    List.iteri (fun i arg ->
      if i > 0 then
        pp_print_break fmt 1 0;
      pp_expr fmt arg
    ) lst;
    pp_close_box fmt ()

and pp_binding fmt (name, typing, value) =
  match typing with
  | None -> fprintf fmt "@[<hov 1>[%s@ %a@,]@]" name pp_expr value
  | Some typing -> fprintf fmt "@[<hov 1>[%s : %a@ %a@,]@]" name pp_typing typing pp_expr value

and pp_expr fmt = function
  | Let { name; typing; value; result } ->
    fprintf fmt "@[<hov 2>(let %a@ %a@,)@]" pp_binding (name, typing, value) pp_expr result
  | Conditional { cond; consequent; alternative } ->
    fprintf fmt "@[<hov 2>(if %a@ @[<hv>%a@ %a@]@,)@]"
      pp_expr cond pp_expr consequent pp_expr alternative
  | Lambda { params; body } ->
    fprintf fmt "@[<hov 2>(lambda %a@ %a@,)@]" pp_string_list params pp_expr body
  | FnApplication { fn; args } ->
    fprintf fmt "@[<hov 2>(%a%a@,)@]" pp_expr fn pp_args_list args
  | BinOp { op; left; right } ->
    fprintf fmt "@[<hov 2>(%a@ %a@ %a@,)@]" pp_bin_op op pp_expr left pp_expr right
  | Literal lit -> pp_literal fmt lit
  | Ident id -> pp_print_string fmt id
  | List lst -> pp_list ~l:'[' ~r:']' fmt pp_expr lst

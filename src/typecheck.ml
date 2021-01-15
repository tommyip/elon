open Containers
open Ast

let src = Logs.Src.create "tychk"
module Log = (val Logs.src_log src : Logs.LOG)

type ty
  = TyUnit
  | TyBool
  | TyInt
  | TyFloat
  | TyChar
  | TyString
  | TyFn of { params: ty list; return: ty }
[@@deriving eq]


let rec pp_ty fmt ty =
  let open Format in
  match ty with
  | TyUnit -> pp_print_string fmt "unit"
  | TyBool -> pp_print_string fmt "bool"
  | TyInt -> pp_print_string fmt "int"
  | TyFloat -> pp_print_string fmt "float"
  | TyChar -> pp_print_string fmt "char"
  | TyString -> pp_print_string fmt "string"
  | TyFn { params; return } ->
      fprintf fmt "@[<hov 0>%a@,->@,%a@]" (pp_list ~l:'(' ~r:')' pp_ty) params pp_ty return

let resolve_ty { name; _ } =
  match name with
  | "unit" -> TyUnit
  | "bool" -> TyBool
  | "int" -> TyInt
  | "float" -> TyFloat
  | "char" -> TyChar
  | "string" -> TyString
  | _ -> failwith "Unimplemented"

type error
  = BinOpArgsMismatch of { op: bin_op; expected: ty * ty; actual: ty * ty }
  | TypeMismatch of { expr: expr; expected: ty; actual: ty }
  | NotAFunction of { expr: expr }
  | ArityMismatch of { params: ty list; args: Ast.expr list }
  | UndefinedIdent of string
  | Unimplemented
[@@deriving show { with_path = false }]

type ty_expr = ty * expr
and expr
  = TyConditional of { cond: expr; consequent: expr; alternative: expr }
  | TyLambda of { params: string list; body: expr }
  | TyApplication of { fn: expr; args: expr list }
  | TyLiteral of literal
  | TyIdent of string

let bin_op_ty = function
  | Add | Sub | Mul | Div -> (TyInt, TyInt, TyInt)
  | Eq | NotEq | Lt | Gt | LtEq | GtEq -> (TyInt, TyInt, TyBool)

type symtbl_entry =
  { ty: ty;
  }

module SymTbl = Map.Make(String)

let rec check expr symtbl =
  let open CCResult in
  match expr with
  | Let { name; typing; value; result } ->
      let* value_ty, value_expr = check value symtbl in
      let* () = match typing with
        | Some annotated_ty ->
            let expected_ty = resolve_ty annotated_ty in
            if equal_ty expected_ty value_ty then Ok ()
            else Error (TypeMismatch { expr=value; expected=expected_ty; actual=value_ty })
        | None -> Ok ()
      in
      let* result_ty, result_expr = check result (SymTbl.add name { ty=value_ty } symtbl) in
      let lambda = TyLambda { params=[name]; body=result_expr } in
      Ok (result_ty, TyApplication { fn=lambda; args=[value_expr] })
  | BinOp { op; left; right } ->
      let* l_ty, l_expr = check left symtbl in
      let* r_ty, r_expr = check right symtbl in
      let expected_l_ty, expected_r_ty, return_ty = bin_op_ty op in
      if equal_ty l_ty expected_l_ty && equal_ty r_ty expected_r_ty then
        Ok (return_ty, TyApplication { fn=TyIdent (bin_op_fn_name op); args=[l_expr; r_expr] })
      else
        Error (BinOpArgsMismatch { op; expected=(expected_l_ty, expected_r_ty); actual=(l_ty, r_ty) })
  | Conditional { cond; consequent; alternative } ->
      let* cond_ty, cond_expr = check cond symtbl in
      if equal_ty cond_ty TyBool then
        let* consequent_ty, consequent_expr = check consequent symtbl in
        let* alternative_ty, alternative_expr = check alternative symtbl in
        if equal_ty consequent_ty alternative_ty then
          Ok (consequent_ty, TyConditional {
            cond=cond_expr;
            consequent=consequent_expr;
            alternative=alternative_expr
          })
        else
          Error (TypeMismatch { expr=alternative; expected=consequent_ty; actual=alternative_ty })
      else
        Error (TypeMismatch { expr=cond; expected=TyBool; actual=cond_ty })
  | Lambda (Untyped _) -> Error Unimplemented
  | Lambda (Typed { params; return; body }) ->
      let params_and_ty = List.to_seq params |> Seq.map (Pair.map_snd resolve_ty) in
      let params_and_entry = Seq.map (Pair.map_snd (fun ty -> { ty })) params_and_ty in
      let* body_ty, body_expr = check body (SymTbl.add_seq symtbl params_and_entry) in
      let return = resolve_ty return in
      if equal_ty body_ty return then
        let params, params_ty = Seq.unzip params_and_ty |> Pair.map List.of_seq List.of_seq in
        Ok (TyFn { params=params_ty; return }, TyLambda { params; body=body_expr })
      else
        Error (TypeMismatch { expr=body; expected=return; actual=body_ty })
  | FnApplication { fn; args } ->
      let* fn_type, fn_expr = check fn symtbl in
      begin match fn_type with
      | TyFn { params; return } ->
        if List.compare_lengths params args = 0 then
          let* args = List.fold_left2
            (fun lst_result param_ty arg ->
              let* lst = lst_result in
              let* arg_ty, arg_expr = check arg symtbl in
              if equal_ty param_ty arg_ty then
                Ok (arg_expr :: lst)
              else
                Error (TypeMismatch { expr=arg; expected=param_ty; actual=arg_ty }))
            (Ok []) params args
          in Ok (return, TyApplication { fn=fn_expr; args=args })
        else
          Error (ArityMismatch { params; args })
      | _ -> Error (NotAFunction { expr=fn })
      end
  | Literal lit ->
      let ty = match lit with
        | Unit -> TyUnit
        | Bool _ -> TyBool
        | Int _ -> TyInt
        | Float _ -> TyFloat
        | Char _ -> TyChar
        | String _ -> TyString
      in Ok (ty, TyLiteral lit)
  | Ident ident ->
      begin match SymTbl.find_opt ident symtbl with
      | Some { ty } -> Ok (ty, TyIdent ident)
      | None -> Error (UndefinedIdent ident)
      end
  | _ -> Error Unimplemented

let convert expr =
  match check expr SymTbl.empty with
  | Ok ast -> ast
  | Error err -> Log.err (fun m -> m "%a" pp_error err); exit 1

open Containers
open Alcotest
open Elon
open Tokens

let pp_token_stream fmt ts =
  Format.pp_open_hovbox fmt 0;
  Format.list Tokens.pp fmt ts;
  Format.pp_close_box fmt ()

let token = testable Tokens.pp Tokens.equal
let token_stream = testable pp_token_stream (List.equal Tokens.equal)

let token_gen in_chan =
  Sedlexing.Utf8.from_channel in_chan
  |> Lexer.token
  |> Preparser.filter

let preparse filename =
  IO.with_in filename @@ fun in_chan ->
    let gen = token_gen in_chan in
    List.of_gen @@ fun () ->
      match gen () with
      | (EOF, _, _) -> None
      | (tok, _, _) -> Some tok

let test_let_inline () =
  check token_stream "Insert IN"
    [LET; IDENT "x"; EQ; IDENT "a"; PLUS; IDENT "b"; IN; IDENT "x"]
    (preparse "syntax/let_inline.elon")

let test_let_multiline () =
  check token_stream "Insert IN"
    [LET; IDENT "x"; EQ; LET; IDENT "y"; EQ; INT (Int64.of_int 42); IN;
     IDENT "y"; PLUS; INT (Int64.of_int 1337); IN;
     IDENT "x"; R_ANGLE_BRACKET; INT (Int64.of_int 9000)]
    (preparse "syntax/let_multiline.elon")

let test_unexpected_indent () =
  check_raises "Let body under indent" (Failure "Unexpected indentation")
    @@ fun () -> ignore (preparse "syntax/let_unexpected_indent.elon")

let test_if_inline () =
  check token_stream "Pass through"
    [IF; IDENT "a"; THEN; IDENT "b"; ELSE; IDENT "c"]
    (preparse "syntax/if_inline.elon")

let test_if_multiline () =
  check token_stream "Pass through"
    [IF; IDENT "a"; THEN; IDENT "b"; ELSE; IDENT "c"]
    (preparse "syntax/if_multiline.elon")

let test_if_multiline_else_not_aligned () =
  check_raises "Else not aligned with If" (Failure "`else` not aligned with `if`")
    @@ fun () -> ignore (preparse "syntax/if_multiline_else_not_aligned.elon")

let test_if_unexpected_indent () =
  check_raises "If body under indented" (Failure "Unexpected indentation")
    @@ fun () -> ignore (preparse "syntax/if_unexpected_indent.elon")

let test_lambda_inline () =
  check token_stream "Pass through"
    [L_PAREN; R_PAREN; FAT_ARROW; INT (Int64.of_int 42)]
    (preparse "syntax/lambda_inline.elon")

let test_lambda_multiline () =
  check token_stream "Pass through"
    [L_PAREN; R_PAREN; FAT_ARROW;
     L_PAREN; IDENT "a"; R_PAREN; FAT_ARROW;
     L_PAREN; IDENT "b"; R_PAREN; FAT_ARROW;
     L_PAREN; R_PAREN; FAT_ARROW; IDENT "a"; PLUS; IDENT "b"]
    (preparse "syntax/lambda_multiline.elon")

let test_lambda_undentation () =
  check token_stream "Pass through"
    [L_PAREN; IDENT "x"; COMMA; IDENT "y"; R_PAREN; FAT_ARROW;
     L_PAREN; IDENT "threshold"; R_PAREN; FAT_ARROW;
     IDENT "x"; PLUS; IDENT "y"; GT_EQ; IDENT "threshold"]
    (preparse "syntax/lambda_undentation.elon")

let test_lambda_unexpected_indent () =
  check_raises "Lambda body under indented" (Failure "Unexpected indentation")
    @@ fun () -> ignore (preparse "syntax/lambda_unexpected_indent.elon")

let test_expr_multiline () =
  check token_stream "Pass through"
    [IDENT "a"; PLUS; IDENT "b"; MINUS; IDENT "c"; TIMES; IDENT "d"; SLASH;
     IDENT "e"; EQ; IDENT "f"; GT_EQ; IDENT "g"]
    (preparse "syntax/expr_multiline.elon")

let test_expr_unexpected_indent () =
  check_raises "Multiline expr under indented" (Failure "Unexpected token")
    @@ fun () -> ignore (preparse "syntax/expr_unexpected_indent.elon")

let test_suite = ("preparser", [
  test_case "let inline" `Quick test_let_inline;
  test_case "let multiline" `Quick test_let_multiline;
  test_case "let under indent" `Quick test_unexpected_indent;
  test_case "if inline" `Quick test_if_inline;
  test_case "if multiline" `Quick test_if_multiline;
  test_case "if multiline else not aligned" `Quick test_if_multiline_else_not_aligned;
  test_case "if unexpected indent" `Quick test_if_unexpected_indent;
  test_case "lambda inline" `Quick test_lambda_inline;
  test_case "lambda multiline" `Quick test_lambda_multiline;
  test_case "lambda unexpected indent" `Quick test_lambda_unexpected_indent;
  test_case "lambda undentation" `Quick test_lambda_undentation;
  test_case "expr multiline" `Quick test_expr_multiline;
  test_case "expr unexpected indent" `Quick test_expr_unexpected_indent;
])

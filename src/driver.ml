type ctx =
  { path: string;
  }

let compile ctx =
  let fmt = Format.std_formatter in

  let lexbuf = Sedlexing.Utf8.from_channel (open_in ctx.path) in
  let token_gen = Lexer.token lexbuf |> Preparser.filter in
  Lexer.pp fmt token_gen;
  (*
  let token_gen = Lexer.init (lexbuf ()) in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.prog in
  let ast = parse token_gen in
  Ast.pp_expr fmt ast
  *)

type ctx =
  { path: string;
  }

let compile ctx =
  let fmt = Format.std_formatter in

  let token_gen () =
    Sedlexing.Utf8.from_channel (open_in ctx.path )
    |> Lexer.token
    |> Preparser.filter
  in

  Lexer.pp fmt (token_gen ());
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.prog in
  let ast = parse (token_gen ()) in
  Ast.pp_expr fmt ast

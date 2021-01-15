type ctx =
  { path: string;
  }

let compile ctx =
  let in_chan = Sedlexing.Utf8.from_channel (open_in ctx.path) in
  let token_gen = Lexer.token in_chan |> Preparser.filter in

  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.prog in
  let ast = parse token_gen in
  let ty_ast = Typecheck.convert ast in
  ()

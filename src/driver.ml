type ctx =
  { path: string;
  }

let compile ctx =
  let lexbuf () = Sedlexing.Utf8.from_channel (open_in ctx.path) in
  Lexer.token_stream_pp Format.std_formatter (lexbuf ());
  (*
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.prog in
  let ast = parse (Lexer.token (lexbuf ())) in
  print_endline (Ast.expr_str ast)

  *)

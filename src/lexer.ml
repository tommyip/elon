open Containers
open Tokens

exception LexingError of string * (Lexing.position * Lexing.position)

let utf8_len = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0

let digit = [%sedlex.regexp? '0'..'9']
let integer = [%sedlex.regexp? Opt '-', Plus digit]
let float = [%sedlex.regexp? (integer, '.') | ('.', Plus digit) | (integer, '.', Plus digit)]
let lowercase = [%sedlex.regexp? 'a'..'z']
let uppercase = [%sedlex.regexp? 'A'..'Z']
let ident = [%sedlex.regexp? (lowercase | uppercase | '_'), Star (lowercase | uppercase | digit | '_')]

type tokenizer_state =
  { mutable indent_stack: int CCList.t;
    deferred_tokens: Tokens.t CCDeque.t;
  }

let rec tokenize state lexbuf =
  match CCDeque.take_front_opt state.deferred_tokens with
  | Some tok -> tok
  | None -> begin
    match%sedlex lexbuf with
    | Plus ' ' ->
        if Sedlexing.lexeme_start lexbuf > 0 then
          tokenize state lexbuf
        else
          raise (LexingError ("Unexpected indent at the start of file", Sedlexing.lexing_positions lexbuf))
    | '(' -> L_PAREN
    | ')' -> R_PAREN
    | '{' -> L_BRACKET
    | '}' -> R_BRACKET
    | '<' -> L_ANGLE_BRACKET
    | '>' -> R_ANGLE_BRACKET
    | ',' -> COMMA
    | ':' -> COLON
    | '=' -> EQ
    | "!=" -> BANG_EQ
    | "<=" -> LT_EQ
    | ">=" -> GT_EQ
    | '+' -> PLUS
    | '-' -> MINUS
    | '*' -> TIMES
    | '/' -> SLASH
    | "()" -> UNIT
    | "true" -> BOOL true
    | "false" -> BOOL false
    | "let" -> LET
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | integer -> INT (Int64.of_string_exn (Sedlexing.Utf8.lexeme lexbuf))
    | float -> FLOAT (Float.of_string_exn (Sedlexing.Utf8.lexeme lexbuf))
    | '\'', any, Star Sub(any, '\''),'\'' ->
        (* A char is a unicode extended grapheme cluster which can contain
           multiple codepoints. *)
        let n_codepoints = Sedlexing.lexeme_length lexbuf - 2 in
        let c = Sedlexing.Utf8.sub_lexeme lexbuf 1 n_codepoints in
        if utf8_len c = 1 then CHAR c
        else raise (LexingError (
          "This character literal contains more than one grapheme",
          Sedlexing.lexing_positions lexbuf))
    | '"', Star Sub(any, '"'), '"' ->
        let len = Sedlexing.lexeme_length lexbuf - 2 in
        STRING (Sedlexing.Utf8.sub_lexeme lexbuf 1 len)
    | ident -> IDENT (Sedlexing.Utf8.lexeme lexbuf)
    | '\n', Star ' ' ->
        let curr_indent = Sedlexing.lexeme_length lexbuf - 1 in
        let rec aux dedenting = function
          | prev_indent :: tl when curr_indent > prev_indent ->
            if not dedenting then begin
                CCDeque.push_back state.deferred_tokens INDENT;
                curr_indent :: prev_indent :: tl
            end else
              raise (LexingError ("Unexpected indentation", Sedlexing.lexing_positions lexbuf))
          | prev_indent :: tl when curr_indent < prev_indent ->
            begin
              CCDeque.push_back state.deferred_tokens DEDENT;
              aux true tl
            end
          | stack (* same indentation *) -> stack
        in
        state.indent_stack <- aux false state.indent_stack;
        NEWLINE
    | eof -> EOF
    | _ -> failwith "Unknown token"
  end

(* Capatibility with Menhir revised API *)
let init lexbuf =
  let state = {
    indent_stack=[0];
    deferred_tokens=CCDeque.create ();
  } in
  fun () ->
    let tok = tokenize state lexbuf in
    let start, end_ = Sedlexing.lexing_positions lexbuf in
    (tok, start, end_)

let token_stream_pp fmt lexbuf =
  let token_gen = init lexbuf in
  Format.pp_open_vbox fmt 0;
  let rec aux () =
    let tok, start, end_ = token_gen () in
    Format.fprintf fmt "@[<h 2>%3d:%-3d %3d:%-3d@ %a@]@,"
      start.pos_lnum (start.pos_cnum - start.pos_bol) end_.pos_lnum (end_.pos_cnum - end_.pos_bol)
      Tokens.pp tok;
    match tok with
    | EOF -> ()
    | _ -> aux ()
  in aux ();
  Format.pp_close_box fmt ();

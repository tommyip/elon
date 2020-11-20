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
  { mutable is_newline: bool;
    mutable indent_stack: int CCList.t;
    mutable dedents_to_emit: int;
  }

let rec tokenize state lexbuf =
  if state.dedents_to_emit > 0 then begin
    state.dedents_to_emit <- state.dedents_to_emit - 1;
    DEDENT
  end else match%sedlex lexbuf with
  | Plus ' ' ->
      if Sedlexing.lexeme_start lexbuf > 0 then
        tokenize state lexbuf
      else
        raise (LexingError ("Unexpected indent at the start of file", Sedlexing.lexing_positions lexbuf))
  | '(' -> L_PAREN
  | ')' -> R_PAREN
  | '{' -> L_BRACKET
  | '}' -> R_BRACKET
  | ',' -> COMMA
  | ':' -> COLON
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> TIMES
  | '/' -> SLASH
  | "()" -> UNIT
  | "true" -> BOOL true
  | "false" -> BOOL false
  | integer -> INT (Int64.of_string (Sedlexing.Utf8.lexeme lexbuf))
  | float -> FLOAT (Float.of_string (Sedlexing.Utf8.lexeme lexbuf))
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
  | '\n', Star ' ' ->
      let open CCList in
      let curr_indent = Sedlexing.lexeme_length lexbuf - 1 in
      let prev_indent = hd state.indent_stack in
      if curr_indent > prev_indent then begin
        state.indent_stack <- curr_indent :: state.indent_stack;
        INDENT
      end else if curr_indent < prev_indent then begin
        (* dedents *)
        let prev_depth = length state.indent_stack in
        state.indent_stack <- drop_while ((<) curr_indent) state.indent_stack;
        if hd state.indent_stack = curr_indent then begin
          state.dedents_to_emit <- prev_depth - length state.indent_stack;
          tokenize state lexbuf
        end else
          raise (LexingError ("Unexpected indent size", Sedlexing.lexing_positions lexbuf))
      end else
        (* same indentation *)
        tokenize state lexbuf
  | eof -> EOF
  | _ -> failwith "Unknown token"

(* Capatibility with Menhir revised API *)
let init lexbuf =
  let state = {
    is_newline=true;
    indent_stack=[0];
    dedents_to_emit=0;
  } in
  fun () ->
    let tok = tokenize state lexbuf in
    let start, end_ = Sedlexing.lexing_positions lexbuf in
    (tok, start, end_)

let lexing_position_pp fmt pos =
  let open Lexing in
  Format.fprintf fmt "%d:%d" pos.pos_lnum pos.pos_cnum

let token_stream_pp fmt lexbuf =
  let token_gen = init lexbuf in
  let rec aux () =
    let tok, start, end_ = token_gen () in
    CCFormat.fprintf fmt "%-35s %a->%a\n"
      (Tokens.show tok)
      lexing_position_pp start
      lexing_position_pp end_;
    if tok <> EOF then aux ()
  in aux ()

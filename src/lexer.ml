open Containers
open Lexing

open Tokens
open Helpers

type t = token * Lexing.position * Lexing.position
type gen = unit -> t

exception LexingError of string * (Lexing.position * Lexing.position)

let utf8_len = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0

let digit = [%sedlex.regexp? '0'..'9']
let integer = [%sedlex.regexp? Opt '-', Plus digit]
let float = [%sedlex.regexp? (integer, '.') | ('.', Plus digit) | (integer, '.', Plus digit)]
let lowercase = [%sedlex.regexp? 'a'..'z']
let uppercase = [%sedlex.regexp? 'A'..'Z']
let ident = [%sedlex.regexp? (lowercase | uppercase | '_'), Star (lowercase | uppercase | digit | '_')]

let rec tokenize lexbuf =
  match%sedlex lexbuf with
  | '#', Star (Compl '\n'), '\n' -> tokenize lexbuf
  | Plus (' ' | '\n') -> tokenize lexbuf
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
  | "=>" -> ARROW
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
  | eof -> EOF
  | _ -> failwith "Unknown token"

let pp fmt (tok, start, end_) =
  Format.fprintf fmt "@[<h 2>%3d:%-3d %3d:%-3d@ %a@]@,"
    start.pos_lnum (Helpers.column start) end_.pos_lnum (Helpers.column end_)
    Tokens.pp tok

(* Capatibility with Menhir revised API *)
let token lexbuf () =
  let token = tokenize lexbuf in
  let start, end_ = Sedlexing.lexing_positions lexbuf in
  let out = (token, start, end_) in
  Log.debug (fun m -> m "%a" pp out ~header:"lexer");
  out

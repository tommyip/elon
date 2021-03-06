open Containers
open Lexing
open Helpers
open Tokens

let src = Logs.Src.create "preparser"
module Log = (val Logs.src_log src : Logs.LOG)

let indent_size = 2

(* TODO: How to recognise whether an angle bracket is a binop or a bracket? *)
type brackets
  = Round
  | Curly
  | Square
[@@deriving eq, show { with_path = false }]

type construct
  = Block
  | Let
  | Typing
  | TypeParam
  | If
  | Brackets of brackets
  | Lambda
[@@deriving eq, show { with_path = false }]

type context =
  { construct: construct;
    line: int;
    offside: int;
    line_indent: int;
  }

type state =
  { token: Lexer.gen;
    mutable stack: context list;
    mutable indent: int;
    mutable prev_line: int;
    mutable peekbuf: Lexer.t option;
    mutable delaybuf: Lexer.t option;
  }

let peek state =
  match state.peekbuf with
  | Some tok -> tok
  | None ->
      let tok = state.token () in
      state.peekbuf <- Some tok;
      tok

let next state =
  match state.peekbuf with
  | Some ((EOF, _, _) as eof) -> eof
  | Some tok -> state.peekbuf <- None; tok
  | None -> begin
      match state.token () with
      | (EOF, _, _) as eof -> state.peekbuf <- Some eof; eof
      | tok -> tok
    end

let internal_token token = (token, dummy_pos, dummy_pos)

let peek_pos state =
  let _, lookahead_start, _ = peek state in
  (lookahead_start.pos_lnum, column lookahead_start)

let is_binop token =
  match token with
  | PLUS | MINUS | TIMES | SLASH | EQ | BANG_EQ | L_CHEVRON
  | R_CHEVRON | LT_EQ | GT_EQ -> true
  | _ -> false

module Bracket = struct
  let is_construct = function Brackets _ -> true | _ -> false
  let type_ = function
    | L_PAREN -> Some Round
    | L_BRACE -> Some Curly
    | L_BRACKET -> Some Square
    | _ -> None
  let is_open = Fun.compose type_ Option.is_some
  let is_close = function
    | R_PAREN | R_BRACE | R_BRACKET -> true
    | _ -> false
  let close_of ~type_ token =
    match type_, token with
    | Round, R_PAREN
    | Curly, R_BRACE
    | Square, R_BRACKET -> true
    | _ -> false
end

let is_type_param = function TypeParam -> true | _ -> false

let rec token state =
  let tok, start, _ = peek state in
  let log_push construct offside = Log.debug (fun m ->
    m "<%a> push %a context (offside: %d)" Tokens.pp tok pp_construct construct offside) in
  let log_pop construct = Log.debug (fun m ->
    m "<%a> pop %a context" Tokens.pp tok pp_construct construct) in

  let prev_line = state.prev_line in
  let deferred_token state = state.prev_line <- prev_line; token state in
  let line = start.pos_lnum in
  let col = column start in
  let line_start =
    if line <> state.prev_line then begin
      state.prev_line <- line;
      state.indent <- col;
      true
    end else
      false
  in
  let line_indent =
    if line_start then col
    else
      let { line_indent; _ } = List.hd state.stack in
      line_indent
  in

  match tok, state.stack with
  | EOF, [_] -> next state
  (* This is unnecessary, only for logging purposes *)
  | EOF, { construct; _ } :: tl ->
      log_pop construct;
      state.stack <- tl;
      deferred_token state

  (* Token on offside line of a block context, except the first token of said block *)
  | _tok, { construct=Block; line=block_line; offside; _ } :: _
    when line > block_line && col = offside ->
      (* TODO implement expr sequence *)
      failwith "Not implemented"

  (* Token offside of block, pop block context *)
  | _tok, { construct=Block; offside; _ } :: tl when col < offside ->
      log_pop Block;
      state.stack <- tl;
      deferred_token state

  (* Token on offside line of let
     let x = ...
     _tok
  *)
  | _tok, { construct=Let; offside; _ } :: tl when col = offside ->
      log_pop Let; log_push Block offside;
      state.stack <- { construct=Block; line; offside; line_indent } :: tl;
      internal_token IN

  | LET, { construct=Block; offside; _ } :: _ when col = offside ->
      log_push Let col;
      state.stack <- { construct=Let; line; offside=col; line_indent } :: state.stack;
      next state

  | EQ, { construct=Typing; _ } :: tl ->
      log_pop Typing;
      state.stack <- tl;
      deferred_token state

  | EQ, { construct=Let; line=let_line; offside; _ } :: _ when line = let_line ->
      let eq_tok = next state in
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line || lookahead_col = offside + indent_size then begin
        let line_indent = if lookahead_line = line then line_indent else lookahead_col in
        log_push Block lookahead_col;
        let block = { construct=Block; line=lookahead_line; offside=lookahead_col; line_indent } in
        state.stack <- block :: state.stack;
        eq_tok
      end else
        failwith "Unexpected indentation"

  | COLON, _ when line = prev_line ->
      let colon_tok = next state in
      log_push Typing col;
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line then begin
        state.stack <- { construct=Typing; line; offside=lookahead_col; line_indent } :: state.stack;
        colon_tok
      end else
        failwith "Type annotation should appear on the same line as `:`"

  | IF, _ ->
      log_push If col;
      state.stack <- { construct=If; line; offside=col; line_indent } :: state.stack;
      next state

  | THEN, { construct=If; line=if_line; offside; _ } :: _ when line = if_line ->
      let then_tok = next state in
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line || lookahead_col = offside + indent_size then begin
        let line_indent = if lookahead_line = line then line_indent else lookahead_col in
        log_push Block lookahead_col;
        state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col; line_indent } :: state.stack;
        then_tok
      end else
        failwith "Unexpected indentation"

  (* Close all context up til If *)
  | ELSE, { construct; _ } :: tl when not (equal_construct construct If) ->
      log_pop construct;
      state.stack <- tl;
      deferred_token state

  | ELSE, { construct=If; line=if_line; offside; _ } :: tl ->
      if line = if_line || col = offside then
        let else_tok = next state in
        let lookahead_line, lookahead_col = peek_pos state in
        if lookahead_line = line || lookahead_col = offside + indent_size then begin
          let line_indent = if lookahead_line = line then line_indent else lookahead_col in
          log_pop If; log_push Block lookahead_col;
          state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col; line_indent } :: tl;
          else_tok
        end else
          failwith "Unexpected indentation"
      else
        failwith "`else` not aligned with `if`"

  | tok, _ when Bracket.is_open tok ->
      let bracket_tok = next state in
      let lookahead_line, lookahead_col = peek_pos state in
      if (lookahead_line = line && lookahead_col = col + 1) ||
        lookahead_col = state.indent + indent_size
      then begin
        let brackets = Brackets (Option.get_exn (Bracket.type_ tok)) in
        log_push brackets lookahead_col;
        let brackets = { construct=brackets; line; offside=lookahead_col; line_indent } in
        let line_indent = if lookahead_line = line then line_indent else lookahead_col in
        log_push Block lookahead_col;
        let block = { construct=Block; line=lookahead_line; offside=lookahead_col; line_indent } in
        state.stack <- block :: brackets :: state.stack;
        bracket_tok
      end else
        failwith "Unexpected indentation"

  (* A close bracket should either be inline with the last item of the bracket
     group or align with the open bracket on a newline.

     If a lambda expression, the function body is indented based on line
     indentation of the open bracket.

     let x = (a, b) =>
       a + b
  *)
  | tok, { construct=(Brackets b) as brackets; line=paren_line; line_indent; _ } :: tl
    when Bracket.close_of ~type_:b tok && (line = prev_line || col = line_indent) ->
      let r_bracket_tok = next state in
      let (lookahead_tok, lookahead_start, _) = peek state in
      log_pop brackets;
      begin match b, lookahead_tok with
      | Round, ARROW
      | Round, FAT_ARROW when line = lookahead_start.pos_lnum ->
          let offside = line_indent + indent_size in
          log_push Lambda offside;
          state.stack <- { construct=Lambda; line=paren_line; offside; line_indent } :: tl
      | _ -> state.stack <- tl
      end;
      r_bracket_tok

  | ARROW, _ ->
      let arrow_tok = next state in
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line then begin
        log_push Typing lookahead_col;
        let typing = { construct=Typing; line; offside=lookahead_col; line_indent } in
        state.stack <- typing :: state.stack;
        arrow_tok
      end else
        failwith "Type annotation should appear on the same line as `->`"

  (* () -> type => ... *)
  | FAT_ARROW, { construct=Typing; _ } :: tl ->
      log_pop Typing;
      state.stack <- tl;
      deferred_token state

  | FAT_ARROW, { construct=Lambda; offside; _ } :: tl ->
      let arrow_tok = next state in
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line || lookahead_col = offside then begin
        let line_indent = if lookahead_line = line then line_indent else lookahead_col in
        log_pop Lambda; log_push Block lookahead_col;
        state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col; line_indent } :: tl;
        arrow_tok
      end else
        failwith "Unexpected indentation"

  (* When encounter a one of `)}],`, close all surrounding context uptil a bracket context.  *)
  | tok, { construct; _ } :: tl when Bracket.is_close tok && not (Bracket.is_construct construct) ->
      log_pop construct;
      state.stack <- tl;
      deferred_token state

  | L_CHEVRON, { construct=Typing; _ } :: _
  | L_CHEVRON, { construct=TypeParam; _ } :: _ ->
      let chevron_tok = next state in
      log_push TypeParam col;
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line then begin
        state.stack <- { construct=TypeParam; line; offside=lookahead_col; line_indent } :: state.stack;
        chevron_tok
      end else
        failwith "Unexpected line break"

  | R_CHEVRON, { construct=TypeParam; _ } :: tl ->
      log_pop TypeParam;
      state.stack <- tl;
      next state

  | COMMA, { construct; _ } :: tl
    when not ((Bracket.is_construct construct) || is_type_param construct)->
      log_pop construct;
      state.stack <- tl;
      deferred_token state
  | COMMA, { construct=Brackets _; offside; _ } :: _ ->
      let comma_tok = next state in
      let lookahead_line, lookahead_col = peek_pos state in
      if lookahead_line = line || lookahead_col = offside then begin
        let line_indent = if lookahead_line = line then line_indent else lookahead_col in
        log_push Block lookahead_col;
        state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col; line_indent } :: state.stack;
        comma_tok
      end else
        failwith "Unexpected indentation"

  (* First token of block *)
  | _tok, { construct=Block; line=block_line; offside; _ } :: _
    when line = block_line && col = offside ->
      next state

  (* Multiline expression *)
  | tok, { construct=Block; offside; _ } :: _
    when line_start && is_binop tok && col = offside + indent_size ->
      next state

  (* Not the first token of the line so no need to consider whether it
     is offside. *)
  | _tok, _ when not line_start ->
      next state

  | _ -> failwith "Unexpected token"

let filter gen =
  let state =
    { token=gen;
      stack=[];
      indent=0;
      prev_line=0;
      peekbuf=None;
      delaybuf=None;
    }
  in
  let _, start, _ = peek state in
  state.stack <- [{ construct=Block; line=start.pos_lnum; offside=0; line_indent=0 }];
  fun () ->
    let (tok, _, _) as token = token state in
    Log.debug (fun m -> m "emit %a" Tokens.pp tok);
    token

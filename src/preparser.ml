open Containers
open Lexing
open Helpers
open Tokens

let indent_size = 2

type construct
  = Block
  | Let
  | If
  | Paren
  | Lambda
[@@deriving eq, show { with_path = false }]

type context =
  { construct: construct;
    line: int;
    offside: int;
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

let peek_body state =
  let _, lookahead_start, _ = peek state in
  (lookahead_start.pos_lnum, column lookahead_start)

let is_binop token =
  match token with
  | PLUS | MINUS | TIMES | SLASH | EQ | BANG_EQ | L_ANGLE_BRACKET
  | R_ANGLE_BRACKET | LT_EQ | GT_EQ -> true
  | _ -> false

let rec token state =
  let tok, start, _ = peek state in
  let log_push construct = Log.debug (fun m ->
    m "<%a> push %a context" Tokens.pp tok pp_construct construct ~header:"preparser") in
  let log_pop construct = Log.debug (fun m ->
    m "<%a> pop %a context" Tokens.pp tok pp_construct construct ~header:"preparser") in
  let log_emit tok' = Log.debug (fun m ->
    m "<%a> emit %a" Tokens.pp tok Tokens.pp tok' ~header:"preparser") in

  let deferred_token =
    let prev_line = state.prev_line in
    fun state ->
      state.prev_line <- prev_line;
      token state
  in
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

  match tok, state.stack with
  | EOF, _ -> next state

  (* Token on offside line of a block context, except the first token of said block *)
  | _tok, { construct=Block; line=block_line; offside } :: _
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
      log_pop Let; log_push Block;
      state.stack <- { construct=Block; line; offside } :: tl;
      log_emit IN;
      internal_token IN

  | LET, { construct=Block; offside; _ } :: _ when col = offside ->
      log_push Let;
      state.stack <- { construct=Let; line; offside=col } :: state.stack;
      next state

  | EQ, { construct=Let; line=let_line; offside } :: _ when line = let_line ->
      let eq_tok = next state in
      let lookahead_line, lookahead_col = peek_body state in
      if lookahead_line = line || lookahead_col = offside + indent_size then begin
        log_push Block;
        let block = { construct=Block; line=lookahead_line; offside=lookahead_col } in
        state.stack <- block :: state.stack;
        eq_tok
      end else
        failwith "Unexpected indentation"

  | IF, { construct=Block; offside; _} :: _ when col = offside ->
      log_push If;
      state.stack <- { construct=If; line; offside=col } :: state.stack;
      next state

  | THEN, { construct=If; line=if_line; offside } :: _ when line = if_line ->
      let then_tok = next state in
      let lookahead_line, lookahead_col = peek_body state in
      if lookahead_line = line || lookahead_col = offside + indent_size then begin
        log_push Block;
        state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col } :: state.stack;
        then_tok
      end else
        failwith "Unexpected indentation"

  (* Close all context up til If *)
  | ELSE, { construct; _ } :: tl when not (equal_construct construct If) ->
      log_pop construct;
      state.stack <- tl;
      deferred_token state

  | ELSE, { construct=If; line=if_line; offside } :: tl ->
      if line = if_line || col = offside then
        let else_tok = next state in
        let lookahead_line, lookahead_col = peek_body state in
        if lookahead_line = line || lookahead_col = offside + indent_size then begin
          log_pop If; log_push Block;
          state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col } :: tl;
          else_tok
        end else
          failwith "Unexpected indentation"
      else
        failwith "`else` not aligned with `if`"

  | L_PAREN, { construct=Block; _ } :: _ ->
      log_push Paren;
      state.stack <- { construct=Paren; line; offside=col } :: state.stack;
      next state

  (* If a lambda expression, the function body is indented based on the line
     start of the paren line.

     let x = (a, b) =>
       a + b
  *)
  | R_PAREN, { construct=Paren; line=paren_line; _ } :: tl when line = paren_line ->
      let r_paren_tok = next state in
      let (lookahead_tok, lookahead_start, _) = peek state in
      log_pop Paren;
      begin match lookahead_tok with
      | FAT_ARROW when line = lookahead_start.pos_lnum ->
          log_push Lambda;
          state.stack <- { construct=Lambda; line=paren_line; offside=(state.indent + indent_size) } :: tl
      | _ -> state.stack <- tl
      end;
      r_paren_tok

  | FAT_ARROW, { construct=Lambda; offside; _ } :: tl ->
      let arrow_tok = next state in
      let lookahead_line, lookahead_col = peek_body state in
      if lookahead_line = line || lookahead_col = offside then begin
        log_pop Lambda; log_push Block;
        state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col } :: tl;
        arrow_tok
      end else
        failwith "Unexpected indentation"

  (* First token of block *)
  | _tok, { construct=Block; line=block_line; offside } :: _
    when line = block_line && col = offside ->
      next state

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
  state.stack <- [{ construct=Block; line=start.pos_lnum; offside=0 }];
  fun () -> token state

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
[@@deriving show]
type state =
  { token: Lexer.gen;
    mutable stack: context list;
    mutable peekbuf: Lexer.t option;
    mutable delaybuf: Lexer.t option;
  }

let init token =
  let stack = { construct=Block; line=1; offside=0 } :: [] in
  { token; stack; peekbuf=None; delaybuf=None }

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

let rec token state =
  let tok, start, _ = peek state in
  let log_push construct =
    Log.debug (fun m -> m "<%a> push %a context" Tokens.pp tok pp_construct construct ~header:"preparser") in
  let log_pop construct =
    Log.debug (fun m -> m "<%a> pop %a context" Tokens.pp tok pp_construct construct ~header:"preparser") in

  let line = start.pos_lnum in
  let col = column start in
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
      token state

  (* Token on offside line of let
     let x = ...
     _tok
  *)
  | _tok, { construct=Let; offside; _ } :: tl when col = offside ->
      log_pop Let; log_push Block;
      state.stack <- { construct=Block; line; offside } :: tl;
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
      token state

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

  | L_PAREN, { construct=Block; offside; _ } :: _ when col = offside ->
      log_push Paren;
      state.stack <- { construct=Paren; line; offside } :: state.stack;
      next state

  | R_PAREN, { construct=Paren; line=paren_line; offside } :: tl when line = paren_line ->
      let r_paren_tok = next state in
      let (lookahead_tok, lookahead_start, _) = peek state in
      log_pop Paren;
      begin match lookahead_tok with
      | ARROW when line = lookahead_start.pos_lnum ->
          log_push Lambda;
          state.stack <- { construct=Lambda; line=paren_line; offside } :: tl
      | _ -> state.stack <- tl
      end;
      r_paren_tok

  | ARROW, { construct=Lambda; offside; _ } :: tl ->
      let arrow_tok = next state in
      let lookahead_line, lookahead_col = peek_body state in
      if lookahead_line = line || lookahead_col = offside + indent_size then begin
        log_pop Lambda; log_push Block;
        state.stack <- { construct=Block; line=lookahead_line; offside=lookahead_col } :: tl;
        arrow_tok
      end else
        failwith "Unexpected indentation"

  (* First token of block *)
  | _tok, { construct=Block; line=block_line; offside } :: _
    when line = block_line && col = offside ->
      next state

  (* Non-offside token *)
  | _tok, { line=construct_line; offside; _ } :: _
    when line = construct_line && col > offside ->
      next state

  | _ -> failwith "Unexpected token"

let filter gen =
  let state = init gen in
  fun () -> token state

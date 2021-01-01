open Containers
open Lexing
open Helpers
open Tokens

let indent_size = 2

type construct = Block | Let
[@@deriving show]

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

let rec token state =
  let tok, start, _ = peek state in

  Format.fprintf Format.std_formatter "%a" Tokens.pp tok;
  Format.pp_print_newline Format.std_formatter ();
  Format.pp_print_list pp_context Format.std_formatter state.stack;
  Format.pp_print_newline Format.std_formatter ();

  let line = start.pos_lnum in
  let col = column start in
  match tok, state.stack with
  | EOF, _ -> next state

  (* Token on offside line of a block context *)
  | _tok, { construct=Block; line=block_line; offside } :: _
    when line > block_line && col = offside ->
      (* TODO implement expr sequence *)
      failwith "Not implemented"

  (* Token offside of block, pop block context *)
  | _tok, { construct=Block; offside; _ } :: tl when col < offside ->
      state.stack <- tl;
      token state

  (* Token on offside line of let
     let x = ...
     _tok
  *)
  | _tok, { construct=Let; offside; _ } :: tl when col = offside ->
      state.stack <- { construct=Block; line; offside } :: tl;
      internal_token IN

  | LET, { construct=Block; offside; _ } :: _ when col = offside ->
      state.stack <- { construct=Let; line; offside=col } :: state.stack;
      next state

  | EQ, { construct=Let; line=let_line; offside } :: _ when line = let_line ->
      let eq_tok = next state in
      let _, lookahead_start, _ = peek state in
      let lookahead_line = lookahead_start.pos_lnum in
      let lookahead_col = column lookahead_start in
      if lookahead_line = line || lookahead_col = offside + indent_size then begin
        let block = { construct=Block; line=lookahead_line; offside=lookahead_col } in
        state.stack <- block :: state.stack;
        eq_tok
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

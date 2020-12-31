exception Unreachable

let column position =
  let open Lexing in
  position.pos_cnum - position.pos_bol

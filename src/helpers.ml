exception Unreachable

let column position =
  let open Lexing in
  position.pos_cnum - position.pos_bol

let src = Logs.Src.create "elon.compiler" ~doc:"logs compilation information"
module Log = (val Logs.src_log src : Logs.LOG)

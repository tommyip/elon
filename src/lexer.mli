type t = Tokens.t * Lexing.position * Lexing.position
(** Type expected by Menhir's simplified revised API. *)

type gen = unit -> t

val token : Sedlexing.lexbuf -> gen
val pp : Format.formatter -> t -> unit

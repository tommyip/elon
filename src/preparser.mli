val filter : Lexer.gen -> Lexer.gen
(** [filter gen] Augment token stream from [Lexer.token] with artificial tokens
    derived from token indentations to delimit syntactic constructs. *)

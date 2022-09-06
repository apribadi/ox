open! Prelude

type t

val make : string -> t

val parse_stmt : t -> Ast_stmt.t
val parse_expr : t -> Ast_expr.t
val parse_block : t -> Ast_block.t

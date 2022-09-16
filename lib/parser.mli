open! Prelude

type t

val make : string -> t

val parse_expr : t -> Ast_expr.t
val parse_decl : t -> Ast_decl.t

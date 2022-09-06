module rec Ast_block : sig
  type t =
    { seq : Ast_stmt.t array
    ; jump : Ast_jump.t
    }
end = Ast_block 

and Ast_expr : sig
  type t =
    | Call of Ast_expr.t * Ast_expr.t array
    | If of Ast_expr.t * Ast_block.t * Ast_block.t
    | Int of int
    | Loop of Ast_block.t
    | Op1 of Ast_op1.t * Ast_expr.t
    | Op2 of Ast_op2.t * Ast_expr.t * Ast_expr.t
    | Var of Ast_symbol.t
end = Ast_expr

and Ast_stmt : sig
  type t =
    | Expr of Ast_expr.t
    | Let1 of Ast_symbol.t * Ast_expr.t
end = Ast_stmt

and Ast_jump : sig
  type t =
    | Break
    | None
    | Return0
    | Return1 of Ast_expr.t
end = Ast_jump

include Ast_expr

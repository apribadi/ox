type t =
  | Fun of
      { name : Ast_symbol.t
      ; parameters : Ast_symbol.t array
      ; body : Ast_block.t
      }

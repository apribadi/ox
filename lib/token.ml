type t =
  | Stop
  | Error
  | Symbol
  | Number
  | String
  | Colon
  | Comma
  | Dot
  | Semicolon
  | L_paren
  | L_paren_no_space
  | L_bracket
  | L_bracket_no_space
  | L_brace
  | R_paren
  | R_bracket
  | R_brace
  | Assignment
  | Equal
  | Not_equal
  | Greater_than
  | Greater_than_or_equal
  | Less_than
  | Less_than_or_equal
  | Plus
  | Minus
  | Star
  | Slash
  | Ampersand
  | At
  | Bang
  | Caret
  | Dollar
  | Percent
  | Pipe
  | Query
  | Tilde
  | And
  | Break
  | Elif
  | Else
  | End
  | For
  | Fun
  | If
  | Let
  | Loop
  | Or
  | Return
  | While
[@@immediate]

let equal (a : t) (b : t) =
  a = b

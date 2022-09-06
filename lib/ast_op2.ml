type t =
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

let to_string = function
  | Equal                 -> "=="
  | Not_equal             -> "!="
  | Greater_than          -> ">"
  | Greater_than_or_equal -> "=="
  | Less_than             -> "<"
  | Less_than_or_equal    -> "<="
  | Plus                  -> "+"
  | Minus                 -> "-"
  | Star                  -> "*"
  | Slash                 -> "/"

open! Prelude

module Kind = struct
  type t =
    | Illegal
    | Digit
    | Dot
    | Hash
    | Letter
    | Linebreak
    | Null
    | Operator
    | Punctuation
    | Quote
    | Space
    | Underscore
end

module Kind_table = struct
  module T : sig
    type t

    val make : Kind.t -> t
    val set : t -> Char.t -> Kind.t -> unit
    val get : t -> Char.t -> Kind.t
  end = struct
    type t = bytes

    let make k = Bytes.make 256 (Obj.magic (k : Kind.t) : char)
    external set : t -> Char.t -> Kind.t -> unit = "%bytes_unsafe_set"
    external get : t -> Char.t -> Kind.t = "%bytes_unsafe_get"
  end

  include T

  let global =
    let t = make Illegal in
    set t '\000' Null;
    set t '\n' Linebreak;
    set t '\r' Linebreak;
    set t ' ' Space;
    set t '!' Operator;
    set t '"' Quote;
    set t '#' Hash;
    set t '$' Operator;
    set t '%' Operator;
    set t '&' Operator;
    set t '(' Punctuation;
    set t ')' Punctuation;
    set t '*' Operator;
    set t '+' Operator;
    set t ',' Punctuation;
    set t '-' Operator;
    set t '.' Dot;
    set t '/' Operator;
    set t '0' Digit;
    set t '1' Digit;
    set t '2' Digit;
    set t '3' Digit;
    set t '4' Digit;
    set t '5' Digit;
    set t '6' Digit;
    set t '7' Digit;
    set t '8' Digit;
    set t '9' Digit;
    set t ':' Punctuation;
    set t ';' Punctuation;
    set t '<' Operator;
    set t '=' Operator;
    set t '>' Operator;
    set t '?' Operator;
    set t '@' Operator;
    set t 'A' Letter;
    set t 'B' Letter;
    set t 'C' Letter;
    set t 'D' Letter;
    set t 'E' Letter;
    set t 'F' Letter;
    set t 'G' Letter;
    set t 'H' Letter;
    set t 'I' Letter;
    set t 'J' Letter;
    set t 'K' Letter;
    set t 'L' Letter;
    set t 'M' Letter;
    set t 'N' Letter;
    set t 'O' Letter;
    set t 'P' Letter;
    set t 'Q' Letter;
    set t 'R' Letter;
    set t 'S' Letter;
    set t 'T' Letter;
    set t 'U' Letter;
    set t 'V' Letter;
    set t 'W' Letter;
    set t 'X' Letter;
    set t 'Y' Letter;
    set t 'Z' Letter;
    set t '[' Punctuation;
    set t ']' Punctuation;
    set t '^' Operator;
    set t '_' Underscore;
    set t 'a' Letter;
    set t 'b' Letter;
    set t 'c' Letter;
    set t 'd' Letter;
    set t 'e' Letter;
    set t 'f' Letter;
    set t 'g' Letter;
    set t 'h' Letter;
    set t 'i' Letter;
    set t 'j' Letter;
    set t 'k' Letter;
    set t 'l' Letter;
    set t 'm' Letter;
    set t 'n' Letter;
    set t 'o' Letter;
    set t 'p' Letter;
    set t 'q' Letter;
    set t 'r' Letter;
    set t 's' Letter;
    set t 't' Letter;
    set t 'u' Letter;
    set t 'v' Letter;
    set t 'w' Letter;
    set t 'x' Letter;
    set t 'y' Letter;
    set t 'z' Letter;
    set t '{' Punctuation;
    set t '|' Operator;
    set t '}' Punctuation;
    set t '~' Operator;
    t
end

let[@inline] next__stop (t : Parser_cursor.t) _ _ i j _ token =
  t.token <- token;
  t.token_start <- i;
  t.token_stop <- j

let next__stop_dot t s u i j c =
  if j - i = 1 then
    next__stop t s u i j c Token.Dot
  else
    next__stop t s u i j c Token.Error

let next__stop_symbol t s u i j c =
  let token =
    match j - i with
    | 2 ->
      if String.unsafe_get s (i + 0) = 'i'
      && String.unsafe_get s (i + 1) = 'f'
      then Token.If
      else
      if String.unsafe_get s (i + 0) = 'o'
      && String.unsafe_get s (i + 1) = 'r'
      then Token.Or
      else Token.Symbol
    | 3 ->
      if String.unsafe_get s (i + 0) = 'a'
      && String.unsafe_get s (i + 1) = 'n'
      && String.unsafe_get s (i + 2) = 'd'
      then Token.And
      else
      if String.unsafe_get s (i + 0) = 'e'
      && String.unsafe_get s (i + 1) = 'n'
      && String.unsafe_get s (i + 2) = 'd'
      then Token.End
      else
      if String.unsafe_get s (i + 0) = 'f'
      && String.unsafe_get s (i + 1) = 'o'
      && String.unsafe_get s (i + 2) = 'r'
      then Token.For
      else
      if String.unsafe_get s (i + 0) = 'f'
      && String.unsafe_get s (i + 1) = 'u'
      && String.unsafe_get s (i + 2) = 'n'
      then Token.Fun
      else
      if String.unsafe_get s (i + 0) = 'l'
      && String.unsafe_get s (i + 1) = 'e'
      && String.unsafe_get s (i + 2) = 't'
      then Token.Let
      else Token.Symbol
    | 4 ->
      if String.unsafe_get s (i + 0) = 'e'
      && String.unsafe_get s (i + 1) = 'l'
      && String.unsafe_get s (i + 2) = 's'
      && String.unsafe_get s (i + 3) = 'e'
      then Token.Else
      else
      if String.unsafe_get s (i + 0) = 'e'
      && String.unsafe_get s (i + 1) = 'l'
      && String.unsafe_get s (i + 2) = 'i'
      && String.unsafe_get s (i + 3) = 'f'
      then Token.Elif
      else
      if String.unsafe_get s (i + 0) = 'l'
      && String.unsafe_get s (i + 1) = 'o'
      && String.unsafe_get s (i + 2) = 'o'
      && String.unsafe_get s (i + 3) = 'p'
      then Token.Loop
      else Token.Symbol
    | 5 ->
      if String.unsafe_get s (i + 0) = 'b'
      && String.unsafe_get s (i + 1) = 'r'
      && String.unsafe_get s (i + 2) = 'e'
      && String.unsafe_get s (i + 3) = 'a'
      && String.unsafe_get s (i + 4) = 'k'
      then Token.Break
      else
      if String.unsafe_get s (i + 0) = 'w'
      && String.unsafe_get s (i + 1) = 'h'
      && String.unsafe_get s (i + 2) = 'i'
      && String.unsafe_get s (i + 3) = 'l'
      && String.unsafe_get s (i + 4) = 'e'
      then Token.While
      else Token.Symbol
    | 6 ->
      if String.unsafe_get s (i + 0) = 'r'
      && String.unsafe_get s (i + 1) = 'e'
      && String.unsafe_get s (i + 2) = 't'
      && String.unsafe_get s (i + 3) = 'u'
      && String.unsafe_get s (i + 4) = 'r'
      && String.unsafe_get s (i + 5) = 'n'
      then Token.Return
      else Token.Symbol
    | _ ->
      Token.Symbol
  in
  next__stop t s u i j c token

let next__stop_illegal_character t s u i j c =
  next__stop t s u i (j + 1) c Token.Error

let next__stop_error t s u i j c =
  next__stop t s u i j c Token.Error

let next__stop_null t s u i j c =
  if String.length s = j then
    (* We've read the null byte that exists at the end of the OCaml string
       representation, at least for OCaml's standard backend.  *)
    next__stop t s u i j c Token.Stop
  else
    next__stop t s u i (j + 1) c Token.Error

let next__stop_number t s u i j c =
  next__stop t s u i j c Token.Number

let next__stop_operator t s u i j c =
  let token =
    if j - i = 1 then begin
      match String.unsafe_get s i with
      | '=' -> Token.Assignment
      | '<' -> Token.Less_than
      | '>' -> Token.Greater_than
      | '+' -> Token.Plus
      | '-' -> Token.Minus
      | '*' -> Token.Star
      | '/' -> Token.Slash
      | '&' -> Token.Ampersand
      | 'A' -> Token.At
      | '!' -> Token.Bang
      | '^' -> Token.Caret
      | '$' -> Token.Dollar
      | '%' -> Token.Percent
      | '|' -> Token.Pipe
      | '?' -> Token.Query
      | '~' -> Token.Tilde
      | _ -> Token.Error
    end else if j - i = 2 then begin
      if String.unsafe_get s (i + 1) = '=' then begin
        match String.unsafe_get s i with
        | '=' -> Token.Equal
        | '!' -> Token.Not_equal
        | '<' -> Token.Less_than_or_equal
        | '>' -> Token.Greater_than_or_equal
        | _ -> Token.Error
      end else begin
        Token.Error
      end
    end else begin
      Token.Error
    end
  in
  next__stop t s u i j c token

let next__stop_string t s u i j c =
  next__stop t s u i (j + 1) c Token.String

let next__stop_punctuation t s u i j c =
  let token =
    match c with
    | ':' -> Token.Colon
    | ',' -> Token.Comma
    | ';' -> Token.Semicolon
    | '(' -> Token.L_paren
    | '[' -> Token.L_bracket
    | '{' -> Token.L_brace
    | ')' -> Token.R_paren
    | ']' -> Token.R_bracket
    | '}' -> Token.R_brace
    | _ -> Token.Error
  in
  next__stop t s u i (j + 1) c token

let next__stop_punctuation_no_space t s u i j c =
  let token =
    match c with
    | ':' -> Token.Colon
    | ',' -> Token.Comma
    | ';' -> Token.Semicolon
    | '(' -> Token.L_paren_no_space
    | '[' -> Token.L_bracket_no_space
    | '{' -> Token.L_brace
    | ')' -> Token.R_paren
    | ']' -> Token.R_bracket
    | '}' -> Token.R_brace
    | _ -> Token.Error
  in
  next__stop t s u i (j + 1) c token

let rec next__dot t s u i j _ =
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Dot -> next__dot t s u i j c
  | _ -> next__stop_dot t s u i j c

let rec next__symbol t s u i j _ =
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Letter -> next__symbol t s u i j c
  | Underscore -> next__symbol t s u i j c
  | _ -> next__stop_symbol t s u i j c

let rec next__number t s u i j _ =
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Digit -> next__number t s u i j c
  | Dot -> next__number t s u i j c
  | _ -> next__stop_number t s u i j c

let rec next__operator t s u i j _ =
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Operator -> next__operator t s u i j c
  | _ -> next__stop_operator t s u i j c

let rec next__string t s u i j _ =
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Null -> next__stop_error t s u i j c
  | Quote -> next__stop_string t s u i j c
  | _ -> next__string t s u i j c

let rec next__restart t s u _ j _ =
  let i = j + 1 in
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Illegal -> next__stop_illegal_character t s u i j c
  | Digit -> next__number t s u i j c
  | Dot -> next__dot t s u i j c
  | Hash -> next__comment t s u i j c
  | Letter -> next__symbol t s u i j c
  | Linebreak -> next__restart t s u i j c
  | Null -> next__stop_null t s u i j c
  | Operator -> next__operator t s u i j c
  | Punctuation -> next__stop_punctuation t s u i j c
  | Quote -> next__string t s u i j c
  | Space -> next__restart t s u i j c
  | Underscore -> next__symbol t s u i j c

and next__comment t s u i j _ =
  let j = j + 1 in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Linebreak -> next__restart t s u i j c
  | Null -> next__stop_null t s u i j c
  | _ -> next__comment t s u i j c

let next__start t s u i =
  let j = i in
  let c = String.unsafe_get s j in
  match Kind_table.get u c with
  | Illegal -> next__stop_illegal_character t s u i j c
  | Digit -> next__number t s u i j c
  | Dot -> next__dot t s u i j c
  | Hash -> next__comment t s u i j c
  | Letter -> next__symbol t s u i j c
  | Linebreak -> next__restart t s u i j c
  | Null -> next__stop_null t s u i j c
  | Operator -> next__operator t s u i j c
  | Punctuation -> next__stop_punctuation_no_space t s u i j c
  | Quote -> next__string t s u i j c
  | Space -> next__restart t s u i j c
  | Underscore -> next__symbol t s u i j c

let next (t : Parser_cursor.t) =
  next__start t t.source Kind_table.global t.token_stop

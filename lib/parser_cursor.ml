open! Prelude

type t =
  { source : string
  ; mutable token : Token.t
  ; mutable token_start : int
  ; mutable token_stop : int
  }

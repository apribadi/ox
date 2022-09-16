type t [@@immediate]

val equal : t -> t -> bool

val make : string -> pos:int -> len:int -> t

val to_string : t -> string

module Private : sig
  val hash : string -> pos:int -> len:int -> int64
end

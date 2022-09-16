open! Prelude 

type t = int

let equal (a : t) (b : t) =
  a = b

external hash
    :  string
    -> (int [@untagged])
    -> (int [@untagged])
    -> (int64 [@unboxed]) =
  "ox_hash"
  "ox_hash_untagged_unboxed"
[@@noalloc]

module Key = struct
  type nonrec t = t
  let hash (t : t) = t
  let sexp_of_t (t : t) = Sexp.Atom (sprintf "%x" t)
  let compare (a : t) (b : t) = compare a b
end

let global_table : (t, string) Hashtbl.t =
  Hashtbl.create (module Key)

let make s ~pos ~len =
  let t = Int64.to_int (hash s pos len) in
  if not (Hashtbl.mem global_table t) then
    Hashtbl.set global_table ~key:t ~data:(String.sub s pos len);
  t

let to_string t =
  Hashtbl.find_exn global_table t

module Private = struct
  let hash s ~pos ~len =
    hash s pos len
end

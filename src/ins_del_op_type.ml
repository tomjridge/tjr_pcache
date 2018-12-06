(** A concrete type for insert and delete operations *)

(** An op is either insert or delete. These are the entries that get
   written to disk. *)

(* FIXME now in fs_shared? *)

[@@@ocaml.warning "-39"]
type ('k,'v) op = Insert of 'k * 'v | Delete of 'k [@@deriving yojson]

let op2k = function
  | Insert (k,_v) -> k
  | Delete k -> k

(** A concrete type for insert and delete operations *)

(** An op is either insert or delete. These are the entries that get
   written to disk. *)

(* FIXME now in fs_shared? *)

[@@@ocaml.warning "-39"]
type ('k,'v) op = Insert of 'k * 'v | Delete of 'k [@@deriving bin_io, yojson]

let op2k = function
  | Insert (k,_v) -> k
  | Delete k -> k


(** The type for the abstract view of the DCL. Also required by the
   make_dcl_ops function. NOTE the values are ('k,'v)op, not 'v. *)
type ('k,'v,'map) kvop_map_ops = ('k,('k,'v)op,'map) Tjr_map.map_ops

let default_kvop_map_ops () = 
  let open Tjr_polymap in
  let open Tjr_map in
  { map_empty=empty (Pervasives.compare);
    map_is_empty=is_empty;
    map_add=add;
    map_remove=remove;
    map_find=find_opt;
    map_bindings=bindings}


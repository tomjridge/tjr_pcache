(** The abstract view of the DCL (for testing). *)

(* open Pcache_intf *)
(*
(** The type for the abstract view of the DCL. Also required by the
   make_dcl_ops function. NOTE the values are ('k,'v)op, not 'v. *)
type ('k,'v,'map) kvop_map_ops = ('k,('k,'v)op,'map) Tjr_map.map_ops

(* FIXME default kv map, not kvop; FIXME these are really for testing only *)
let default_kvop_map_ops () : ('k,'v,('k,'v,unit)Tjr_map.map) Tjr_map.map_ops = 
  let open Tjr_map in
  make_map_ops Pervasives.compare

let _ = default_kvop_map_ops


let op_list_to_map ops = 
  let map_ops = default_kvop_map_ops () in
  (ops,map_ops.empty) |> iter_break
    (function
      | ([],m) -> Break m
      | op::ops,m -> match op with
        | Insert(k,v) -> Cont(ops,map_ops.add k v m)
        | Delete k -> Cont(ops,map_ops.remove k m))
*)

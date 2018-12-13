open Ins_del_op_type

(** The type for the abstract view of the DCL. Also required by the
   make_dcl_ops function. NOTE the values are ('k,'v)op, not 'v. *)
type ('k,'v,'map) kvop_map_ops = ('k,('k,'v)op,'map) Tjr_map.map_ops

(* FIXME default kv map, not kvop *)
let default_kvop_map_ops () = 
  let open Tjr_polymap in
  let open Tjr_map in
  { map_empty=empty Pervasives.compare;
    map_is_empty=is_empty;
    map_add=add;
    map_remove=remove;
    map_find=find_opt;
    map_bindings=bindings}


let op_list_to_map ops = 
  List.fold_left
    (fun map op -> 
       match op with
       | Insert(k,v) -> Tjr_polymap.add k v map
       | Delete k -> Tjr_polymap.remove k map)
    (Tjr_polymap.empty Pervasives.compare)
    ops

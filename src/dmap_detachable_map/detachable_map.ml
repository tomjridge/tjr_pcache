(** As {!Detachable_chunked_list}, but with the operations insert and
   delete; and the abstract view a map. *)

open Ins_del_op_type
open Pcl_types
open Detachable_chunked_list

let make_dmap_ops ~monad_ops ~(pcl_ops:('op,'ptr,'t)pcl_ops) ~with_dcl =

  let map_ops = Ins_del_op_type.default_kvop_map_ops () in

  let abs_ops = {
    empty=map_ops.map_empty;
    add=(fun op map ->
        match op with
        | Insert(k,v) -> map_ops.map_add k v map
        | Delete k -> map_ops.map_remove k map);
    merge=(fun old new_ -> 
        Tjr_map.map_union ~map_ops ~m1:old ~m2:new_)
  } 
  in
  make_dcl_ops ~monad_ops ~pcl_ops ~with_dcl ~abs_ops


let _ = make_dmap_ops
  
  

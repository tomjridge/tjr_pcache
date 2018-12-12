(** As {!Detachable_chunked_list}, but with the operations insert and
   delete; and the abstract view a map. *)

open Tjr_monad.Types
open Ins_del_op_type
open Pcl_types
open Detachable_chunked_list

include Dmap_types

let make_dmap_ops 
    ~monad_ops 
    ~(pcl_ops:('op,'ptr,'t)pcl_ops) 
    ~(with_dmap:(('ptr,'k,'v)dmap_state,'t) Tjr_monad.With_state.with_state) 
    : ('ptr,'k,'v,'t) dmap_ops
  =

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
  make_dcl_ops ~monad_ops ~pcl_ops ~with_dcl:with_dmap ~abs_ops


let _ = make_dmap_ops
  
  

let convert_dmap_ops_to_map_ops ~monad_ops ~dmap_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let map_ops = Ins_del_op_type.default_kvop_map_ops () in
  let find k = 
    dmap_ops.peek () >>= fun dcl_state ->
    let map = Tjr_map.map_union ~map_ops ~m1:dcl_state.abs_past ~m2:dcl_state.abs_current in
    return (map_ops.map_find k map)
  in
  let insert k v = dmap_ops.add (Insert(k,v)) in
  let delete k = dmap_ops.add (Delete k) in
  let detach () = 
    dmap_ops.detach () >>= fun dcl_state ->
    return (dcl_state.abs_past,dcl_state.abs_current)
  in
  Dmap_types.{find;insert;delete;detach}
    
      

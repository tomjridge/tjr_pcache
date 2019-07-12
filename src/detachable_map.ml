(** As {!Detachable_chunked_list}, but with the operations insert and
    delete; and the abstract view a map. *)


open Pcache_intf
open Pcl_types
open Dcl_types

open Dmap_types

module Profiler = Make_profiler()
open Profiler

module Internal = struct

  let map_merge ~map_ops ~old ~new_ = (
    let open Tjr_map in
    map_ops.bindings new_ |> fun kvs ->
    (kvs,old) |> List_.iter_opt
      (function
        | ([],m) -> None
        | ((k,v)::kvs,m) -> 
          Some (kvs,map_ops.add k v m))
    |> (fun ([],m) -> m)[@ocaml.warning "-8"]
  )
      
  let make_dmap_dcl_ops 
      ~monad_ops 
      ~(pcl_ops:('op,'ptr,'t)pcl_ops) 
      ~(with_dmap:(('ptr,'k,'v)dmap_state,'t) Tjr_monad.with_state) 
    : ('ptr,'k,'v,'t) dmap_dcl_ops
    =
    let map_ops = Op_aux.default_kvop_map_ops () in

    (* for the abstract view, we can't just use maps, because we need to
       track a delete explicitly (otherwise, merging past and current
       maps will "forget" a deleted item in current); sp instead we use
       a map from k to kvop *) 

    let abs_ops = {
      empty=map_ops.empty;
      add=(fun op map ->        
          match op with
          | Insert(k,v) -> map_ops.add k op map
          | Delete k -> map_ops.add k op map);
      merge=(fun old new_ -> map_merge ~map_ops ~old ~new_)
    } 
    in
    let _ : (('k,'v)op,('k,('k,'v)op,unit)Tjr_map.map)abs_ops = abs_ops in
    Detachable_chunked_list.make_dcl_ops ~monad_ops ~pcl_ops ~with_dcl:with_dmap ~abs_ops


  let _ :
    monad_ops:'t monad_ops ->
    pcl_ops:(('k, 'v) Ins_del_op.op, 'ptr, 't) pcl_ops ->
    with_dmap:(('ptr, 'k, 'v) dmap_state, 't) with_state ->
    ('ptr, 'k, 'v, 't) dmap_dcl_ops
    = make_dmap_dcl_ops


  let convert_dcl_to_dmap ~monad_ops ~dmap_dcl_ops =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let profile_m s m = 
      return () >>= fun () -> 
      mark s;
      m >>= fun r ->
      mark (s^"'");
      return r
    in

    let map_ops = Op_aux.default_kvop_map_ops () in
    let find k = 
      dmap_dcl_ops.peek () >>= fun dcl_state ->
      let map = map_merge ~map_ops ~old:dcl_state.abs_past ~new_:dcl_state.abs_current in
      let v = 
        match map_ops.find_opt k map with
        | None -> None
        | Some(op) -> (
            match op with
            | Insert (k,v) -> Some v
            | Delete k -> None)
      in        
      return v
    in
    let insert k v = profile_m "insert" @@ dmap_dcl_ops.add (Insert(k,v)) in
    let delete k = profile_m "delete" @@ dmap_dcl_ops.add (Delete k) in
    let detach () = 
      profile_m "detach" @@ 
      dmap_dcl_ops.detach () >>= fun dcl_state ->
      return { past_map=dcl_state.abs_past;
               current_map=dcl_state.abs_current;
               current_ptr=dcl_state.current_block }
    in
    let block_list_length () =
      dmap_dcl_ops.block_list_length ()
    in
    Dmap_types.{find;insert;delete;detach;block_list_length}


end
open Internal

let make_dmap_dcl_ops,convert_dcl_to_dmap = make_dmap_dcl_ops,convert_dcl_to_dmap


(** A dummy implementation of the dmap_as_map interface, for testing.

For block_list_length, we store the number of ops and perform some
   silly calculation to simulate moving to a new block.

*)

open Tjr_map

open Ins_del_op


module Dummy_state = struct
  type ('k,'v,'ptr) t = {
    map_past: ('k,'v)kvop_map;
    map_current: ('k,'v)kvop_map;
    op_count_current: int;
    ptr_current: 'ptr;
    block_list_length: int;
  }

  let init_dummy_state ~init_ptr = 
    let map_ops = Tjr_map.make_map_ops Pervasives.compare in
    {
      map_past = map_ops.empty;
      map_current = map_ops.empty;
      op_count_current=0;
      ptr_current=init_ptr;
      block_list_length=1
    }

  let _ = init_dummy_state

end

open Dummy_state

let map_merge = Tjr_map.map_merge

let make_dmap_ops ~monad_ops ~with_state ~ops_per_block ~alloc_ptr
  : ('k,'v,'ptr,'t)Dmap_types.dmap_ops 
  = 
  let return = monad_ops.return in
  let ( >>= ) = monad_ops.bind in
  let map_ops = Tjr_map.make_map_ops Pervasives.compare in
  let with_state = with_state.with_state in
  let find k =
    with_state (fun ~state:s ~set_state:_ -> 
      let map = map_merge ~map_ops ~old:s.map_past ~new_:s.map_current in
      let op = map_ops.find_opt k map in
      return (
        match op with
        | None -> None
        | Some(Insert(_,v)) -> Some v
        | Some(Delete _) -> None))
  in
  let maybe_move_to_new_block s = 
    match s.op_count_current > ops_per_block with
    | true -> alloc_ptr () >>= fun ptr -> return {
        map_current=map_ops.empty; 
        op_count_current=0;
        ptr_current=ptr;
        map_past=(map_merge ~map_ops ~old:s.map_past ~new_:s.map_current);
        block_list_length=s.block_list_length+1 }
    | false -> return s
  in
  let insert k v = 
    with_state (fun ~state:s ~set_state -> 
      let s = { s with 
                map_current=map_ops.add k (Insert(k,v)) s.map_current;
                op_count_current=s.op_count_current+1;
              }
      in
      (* maybe move to a new block *)
      maybe_move_to_new_block s >>= fun s -> 
      set_state s)
  in
  let delete k =
    with_state (fun ~state:s ~set_state -> 
      let s = { s with 
                map_current=map_ops.add k (Delete(k)) s.map_current;
                op_count_current=s.op_count_current+1; }
      in
      (* maybe move to a new block *)
      maybe_move_to_new_block s  >>= fun s ->
      set_state s)
  in
  let detach () =
    with_state (fun ~state:s ~set_state -> 
      let new_state = { 
        map_past=map_ops.empty;
        map_current=s.map_current;
        op_count_current=s.op_count_current;
        ptr_current=s.ptr_current;
        block_list_length=1}
      in
      set_state new_state >>= fun () ->
      let open Dmap_types in
      return { 
        past_map=s.map_past; 
        current_map=s.map_current; 
        current_ptr=s.ptr_current })
  in
  let block_list_length () = with_state (fun ~state:s ~set_state:_ -> 
      return s.block_list_length)
  in
  let dmap_write () = return () in
  let dmap_sync = dmap_write in 
  Dmap_types.{find;insert;delete;detach;block_list_length; dmap_write; dmap_sync }

let _ = make_dmap_ops

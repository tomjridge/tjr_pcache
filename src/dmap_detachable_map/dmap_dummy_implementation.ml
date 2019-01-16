(** A dummy implementation of the dmap_as_map interface, for testing.

For block_list_length, we store the number of ops and perform some
   silly calculation to simulate moving to a new block.

*)

open Tjr_map
open Tjr_monad.Types
open Ins_del_op_type


module Dummy_state = struct
  type ('k,'v,'ptr) t = {
    map_past: ('k,('k,'v)op) Tjr_polymap.t;
    map_current: ('k,('k,'v)op) Tjr_polymap.t;
    op_count_current: int;
    ptr_current: 'ptr;
    block_list_length: int;
  }

  let init_dummy_state ~init_ptr = 
    let map_ops = Op_aux.default_kvop_map_ops () in
    {
      map_past = map_ops.map_empty;
      map_current = map_ops.map_empty;
      op_count_current=0;
      ptr_current=init_ptr;
      block_list_length=1
    }

end

open Dummy_state

let make_dmap_ops ~monad_ops ~with_state ~ops_per_block ~alloc_ptr 
  : ('k,'v,'ptr,'t)Dmap_types.dmap_ops 
  = 
  let return = monad_ops.return in
  let ( >>= ) = monad_ops.bind in
  let map_ops = Op_aux.default_kvop_map_ops () in
  let with_state = with_state.with_state in
  let find k =
    with_state (fun ~state:s ~set_state -> 
        let map = Tjr_map.map_union ~map_ops ~m1:s.map_past ~m2:s.map_current in
        let op = map_ops.map_find k map in
        return (
          match op with
          | None -> None
          | Some(Insert(_,v)) -> Some v
          | Some(Delete _) -> None))
  in
  let maybe_move_to_new_block s = 
    match s.op_count_current > ops_per_block with
    | true -> alloc_ptr >>= fun ptr -> return {
        map_current=map_ops.map_empty; 
        op_count_current=0;
        ptr_current=ptr;
        map_past=(Tjr_polymap.union s.map_past s.map_current);
        block_list_length=s.block_list_length+1 }
    | false -> return s
  in
  let insert k v = 
    with_state (fun ~state:s ~set_state -> 
        let s = { s with 
                  map_current=map_ops.map_add k (Insert(k,v)) s.map_current;
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
                  map_current=map_ops.map_add k (Delete(k)) s.map_current;
                  op_count_current=s.op_count_current+1;
                }
        in
        (* maybe move to a new block *)
        maybe_move_to_new_block s  >>= fun s ->
        set_state s)
  in
  let detach () =
    with_state (fun ~state:s ~set_state -> 
        let new_state = { 
          map_past=map_ops.map_empty;
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
  let block_list_length () = with_state (fun ~state:s ~set_state -> 
      return s.block_list_length)
  in
  Dmap_types.{find;insert;delete;detach;block_list_length}

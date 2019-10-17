open Pcache_intf

module type C_rt = sig
  type t
  val monad_ops: t monad_ops
  type r [@@deriving bin_io]
  val blk_alloc: unit -> (r,t) m
end

type ('k,'v,'t) map_ops = {
  empty  : 't;
  find   : 'k -> 't -> 'v option;
  insert : 'k -> 'v -> 't -> 't;
  delete : 'k -> 't -> 't;
  merge  : older:'t -> newer:'t -> 't; 
}

type ('k,'v,'ptr) marshalling_config = {
  ptr_sz        : int;
  blk_sz        : int;
  k_size        : int;
  v_size        : int;
  k_writer      : 'k Bin_prot.Type_class.writer;
  k_reader      : 'k Bin_prot.Type_class.reader;
  v_writer      : 'v Bin_prot.Type_class.writer;
  v_reader      : 'v Bin_prot.Type_class.reader;
  ptr_writer    : 'ptr Bin_prot.Type_class.writer;
  ptr_reader    : 'ptr Bin_prot.Type_class.reader;
}

module type C_dmap = sig
  val create_buf : int -> buf
  include C_rt
  type k [@@deriving bin_io]
  type v [@@deriving bin_io]
  type nonrec kvop = (k,v)kvop [@@deriving bin_io]
  type kvop_map
  val map_ops: (k,v,kvop_map)map_ops
  type nonrec dmap_state = (r,kvop_map) dmap_state
  type blk_id = r
  val blk_ops: blk_id blk_ops
  val spec_add: kvop -> (unit,t)m
  val spec_detach: kvop list -> (unit,t)m  (* e list should be the current contents of the node *)
  val marshalling_config : (k,v,r)marshalling_config
  val with_dmap: (dmap_state,t)with_state
end


module Make(S:C_dmap) = struct
  open S

  let return = monad_ops.return
  let ( >>= ) = monad_ops.bind

(*
  module Elt = struct 
    open Bin_prot.Std
    type elt = Insert of k*v | Delete of k | Next of r [@@deriving bin_io]

    (* gives bin_reader_elt and bin_writer_elt *)
  end
  open Elt
*)

  module Nxt = struct 
    open Bin_prot.Std
    type nxt = r option [@@deriving bin_io]
    (* gives bin_reader_nxt and bin_writer_nxt *)
  end
  open Nxt


  let c = marshalling_config

  let next_ptr_byte_size = c.ptr_sz+1 

  let op_byte_size = max (c.k_size + c.v_size + 1) (c.ptr_sz + 1)

  let find k = with_dmap.with_state (fun ~state ~set_state:_ -> 
      map_ops.find k state.current_map |> function
      | Some v -> return (Some v)
      | None -> map_ops.find k state.past_map |> return)

  (* we write the next ptr at position buf_sz - next_ptr_byte_size *)

  let set_next_ptr r s = 
    let { buf; _ } = s in
    let _n = 
      bin_writer_nxt.write buf ~pos:(c.blk_sz - next_ptr_byte_size)
    in
    { s with buf; dirty=true }

  let alloc_next_ptr_and_set s = 
    assert (s.next_ptr = None);
    blk_alloc () >>= fun r -> 
    return (r,set_next_ptr r s)

  let write_buf buf = return () (* FIXME *)
  
  let add_in_current_node op state =
    assert(state.buf_pos + op_byte_size + next_ptr_byte_size < c.blk_sz);
    (* write into buf, and adjust the state *)
    let { current_map; buf; buf_pos; dirty; _ } = state in
    let current_map = 
      match op with
      | Insert(k,v) -> map_ops.insert k v current_map 
      | Delete k -> map_ops.delete k current_map
    in
    let buf_pos =
      bin_writer_kvop.write buf ~pos:buf_pos op (* note this mutates! *)
    in
    let dirty = true in
    { state with current_map; buf; buf_pos; dirty }
    
  let add op = 
    with_dmap.with_state (fun ~state ~set_state -> 
      (* can we insert in the current node? *)
      match state.buf_pos + op_byte_size + next_ptr_byte_size < c.blk_sz with
      | true -> (
          add_in_current_node op state |> fun state -> 
          set_state state)
      | false -> (
          (* we need to allocate a new node if not already allocated,
             issue a write for the current node then move to the next *)
          alloc_next_ptr_and_set state >>= fun (r,state) ->
          write_buf state.buf >>= fun () ->
          let state = { state with 
                        current_ptr=r;
                        current_map=map_ops.empty;
                        buf=create_buf c.blk_sz;
                        buf_pos=0;
                        next_ptr=None;
                        block_list_length=state.block_list_length+1;
                        dirty=true }
          in
          add_in_current_node op state |> fun state ->
          set_state state))
        
  let insert k v = add (Insert(k,v))
  let delete k = add (Delete k)

  let detach () = 
    with_dmap.with_state (fun ~state ~set_state ->
      assert(state.block_list_length >= 1);
      match state.block_list_length with
      | 1 -> (
          (* the case when there is only one block *)
          let { root_ptr; past_map; current_ptr; current_map; _ } = state in
          return { root_ptr; past_map; current_ptr; current_map })
      | _ -> (
          let { root_ptr; past_map; current_ptr; current_map; _ } = state in
          let detach_info = { root_ptr; past_map; current_ptr; current_map } in          
          let state = { state with root_ptr=current_ptr;
                                   past_map=map_ops.empty }
          in
          set_state state >>= fun () -> 
          return detach_info))

  let block_list_length () = with_dmap.with_state (fun ~state:s ~set_state:_  ->
      return s.block_list_length)

  let dmap_write () = with_dmap.with_state (fun ~state:s ~set_state:_  ->
      write_buf s.buf)

  let dmap_sync = dmap_write
    
end


(* when next_ptr gets set, it automatically gets marshalled into last 10 bytes of buf *)

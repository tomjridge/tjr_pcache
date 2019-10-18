open Pcache_intf

type ('k,'v,'t) map_ops = {
  empty    : 't;
  find_opt : 'k -> 't -> 'v option;
  insert   : 'k -> 'v -> 't -> 't;
  delete   : 'k -> 't -> 't;
  merge    : older:'t -> newer:'t -> 't; 
}


module type C_dmap = sig
  type t
  val monad_ops: t monad_ops
  type k
  type v
  type r
  type nonrec kvop = (k,v)kvop
  val marshalling_config: (k,v,r) marshalling_config
  type kvop_map
  val map_ops: (k,v,kvop_map)map_ops
  type nonrec dmap_state = (r,kvop_map) dmap_state
  type blk_id = r
  (* this should make sure to initialize the nxt pointer, or else we
     require that None is marshalled to 1 or more 0 bytes *)
  type blk
  val blk_ops: blk blk_ops
  val blk_alloc: unit -> (r,t) m
  val with_dmap: (dmap_state,t)with_state
  val write_to_disk: dmap_state -> (unit,t)m
  type nonrec dmap_ops = (k,v,r,kvop_map,t) dmap_ops
end

type ('a,'b,'c,'d,'e,'f,'g) c_dmap = {
  monad_ops:'a;
  marshalling_config:'b;
  map_ops:'c;
  blk_ops:'d;
  blk_alloc:'e;
  with_dmap:'f;
  write_to_disk:'g
}

module Make(S:C_dmap) : sig val dmap_ops: S.dmap_ops end = struct
  open S

  let return = monad_ops.return
  let ( >>= ) = monad_ops.bind

  let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int

  module S = (val marshalling_config : MC with type k=k and type v=v and type r=r)
  open S

  module Nxt = struct 
    open Bin_prot.Std
    type nxt = r option [@@deriving bin_io]
    let nxt_size = r_size + 1
    (* gives bin_reader_nxt and bin_writer_nxt *)
  end
  open Nxt

  (* check that None marshalls to the empty byte *)
  let _ = 
    buf_ops.create blk_sz |> fun buf ->
    bin_write_nxt buf ~pos:0 None |> fun n ->
      assert (n=1);
      assert (Char.equal (buf_ops.get 0 buf) (Char.chr 0))

  (* let next_ptr_byte_size = r_size *)

  let op_byte_size = k_size + v_size + 1

  let find k = with_dmap.with_state (fun ~state ~set_state:_ -> 
      map_ops.find_opt k state.current_map |> function
      | Some v -> return (Some v)
      | None -> map_ops.find_opt k state.past_map |> return)

  (* we write the next ptr at position buf_sz - next_ptr_byte_size *)

  (* when next_ptr gets set, it automatically gets marshalled into last nxt_size bytes of buf *)
  let set_next_ptr r s = 
    let { buf; _ } = s in
    let _n = 
      bin_writer_nxt.write buf ~pos:(blk_sz - nxt_size)
    in
    { s with next_ptr=(Some r); buf; dirty=true }

  let alloc_next_ptr_and_set s = 
    assert (s.next_ptr = None);
    blk_alloc () >>= fun r -> 
    return (r,set_next_ptr r s)

  let can_fit n state = state.buf_pos + n < blk_sz - nxt_size

  let add_in_current_node op state =
    assert(can_fit op_byte_size state);
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
      match can_fit op_byte_size state with
      | true -> (
          add_in_current_node op state |> fun state -> 
          set_state state)
      | false -> (
          (* we need to allocate a new node if not already allocated,
             issue a write for the current node then move to the next *)
          alloc_next_ptr_and_set state >>= fun (r,state) ->
          write_to_disk state >>= fun () ->
          let state = { root_ptr=state.root_ptr;
                        past_map=map_ops.merge ~older:state.past_map ~newer:state.current_map;
                        current_ptr=r;
                        current_map=map_ops.empty;
                        buf=buf_ops.create blk_sz;
                        buf_pos=0;
                        next_ptr=None;
                        block_list_length=state.block_list_length+1;
                        dirty=true }
          in
          (* Printf.printf "buf_pos: %d\n" state.buf_pos; *)
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
                                   past_map=map_ops.empty;
                                   block_list_length=1 }
          in
          set_state state >>= fun () -> 
          return detach_info))

  let block_list_length () = with_dmap.with_state (fun ~state:s ~set_state:_  ->
      return s.block_list_length)

  let dmap_write () = with_dmap.with_state (fun ~state:s ~set_state:_  ->
      write_to_disk s)

  let dmap_sync = dmap_write

  let dmap_ops = { find; insert; delete; detach; block_list_length; dmap_write; dmap_sync }
    
end


(** alternative based on fixing just the types; halfway house between functor and function *)
module Make_with_fixed_types(S: sig
    type t 
    type k 
    type v 
    type r
    (* type nonrec kvop = (k,v)kvop *)
    type kvop_map
    type blk
    (* type nonrec dmap_state = (r,kvop_map) dmap_state *)
    (* type blk_id = r *)
    (* type nonrec dmap_ops = (k,v,r,kvop_map,t) dmap_ops *)
      
end) = struct
  open S

  let make c_dmap = 
    let module A = struct
      type nonrec t=t
      type nonrec k=k
      type nonrec v=v
      type nonrec r=r
      type nonrec kvop = (k,v)kvop
      type nonrec kvop_map = kvop_map
      type nonrec dmap_state = (r,kvop_map) dmap_state
      type blk_id = r
      type nonrec blk = blk
      type nonrec dmap_ops = (k,v,r,kvop_map,t) dmap_ops
      let { monad_ops; marshalling_config; map_ops; blk_ops; blk_alloc; with_dmap; write_to_disk } = c_dmap
    end
    in 
    let module B = Make(A) in
    B.dmap_ops

  let _ :
(t monad_ops, (k, v, r) marshalling_config, (k, v, S.kvop_map) map_ops,
 blk blk_ops, unit -> (r, t) m, ((r, S.kvop_map) dmap_state, t) with_state,
 (r, S.kvop_map) dmap_state -> (unit, t) m)
c_dmap -> (k, v, r, S.kvop_map, t) dmap_ops
= make
end


(** make as a function *)
let make (type t k v r kvop_map blk) c_dmap = 
  let module A = struct
    type nonrec t=t
    type nonrec k=k
    type nonrec v=v
    type nonrec r=r
    type nonrec kvop_map=kvop_map
    type nonrec blk = blk
  end
  in 
  let module B = Make_with_fixed_types(A) in
  B.make c_dmap

let _ :
('a monad_ops, ('b, 'c, 'd) marshalling_config, ('b, 'c, 'e) map_ops,
 'f blk_ops, unit -> ('d, 'a) m, (('d, 'e) dmap_state, 'a) with_state,
 ('d, 'e) dmap_state -> (unit, 'a) m)
c_dmap -> ('b, 'c, 'd, 'e, 'a) dmap_ops
= make

    
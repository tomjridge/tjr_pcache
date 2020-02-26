(** A simplified "Make" functor, to construct a persistent cache. *)

open Pcache_intf
(* open Pcache_state *)

module type S = sig
  include MRSHL
  val k_cmp: k -> k -> int
end

module Make_1(S:S) = struct  
  (* open S *)

  (* what we use with Tjr_pcache.Make *)
  module S2 = struct
    include S

    type t = lwt

    let monad_ops = lwt_monad_ops

    type nonrec kvop = (k, v) Tjr_fs_shared.kvop
    
    let marshalling_config : (k,v,r)marshalling_config = (module S)

    type blk_id = r

    type blk = ba_buf

    let blk_ops = Blk_factory.make_3 ()
  end
  include S2
  
  module Pcache = Tjr_pcache.Make(S2)
  include Pcache

  let make 
      ~(blk_alloc:(unit -> (r,t)m))
      ~(with_dmap:(dmap_state,t)with_state) 
      ~(write_to_disk:dmap_state -> (unit,t)m)
    : Pcache.pcache_ops = 
    Pcache.make_pcache_ops ~blk_alloc ~with_dmap ~write_to_disk

  let _ = make
    
  (* let empty_dmap_state ~root_ptr ~current_ptr = Pcache.empty_dmap_state S2.empPcache_state.empty_dmap_state ~root_ptr ~current_ptr ~empty:empty_kvop_map *\) *)

(*
  module Pvt : sig 
    type pcache_descr = private dmap_ops
  end = struct
    type pcache_descr = dmap_ops
  end
*)
(*
  type pcache_descr = { 
    (* blk_alloc:(unit -> (r,t)m);  *)
    (* with_dmap:(dmap_state,t)with_state; *)
    (* write_to_disk:dmap_state -> (unit,t)m; *)
    dmap_ops:dmap_ops
  }

  let find ~pd = pd.dmap_ops.find 
  let insert ~pd = pd.dmap_ops.insert
  let delete ~pd = pd.dmap_ops.delete
  let detach ~pd = pd.dmap_ops.detach ()
  let block_list_length ~pd = pd.dmap_ops.block_list_length
  let dmap_write ~pd = pd.dmap_ops.dmap_write
  let dmap_sync ~pd = pd.dmap_ops.dmap_sync ()
  let read_pcache ~pd = pd.dmap_ops.read_pcache
*)
end


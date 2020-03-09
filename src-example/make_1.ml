(** Simplified, providing an object version *)
open Pcache_intf

(* module Std_types = Std_types *)
(* open Std_types *)

module type S = sig
  include MRSHL 
  val k_cmp: k -> k -> int
end

module Make(S:S) = struct  
  (* open S *)

  (* what we use with Tjr_pcache.Make *)
  module S2 = struct
    include S

    type t = Std_types.t

    let monad_ops = lwt_monad_ops

    type nonrec kvop = (k, v) Tjr_fs_shared.kvop
    
    module Mrshl = struct
      include S
      type r = Std_types.r[@@deriving bin_io, yojson]
      let r_size = 9
    end

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
      ~(with_pcache:(pcache_state,t)with_state) 
      ~(flush_tl:pcache_state -> (unit,t)m)
    : Pcache.pcache_ops = 
    Pcache.make_pcache_ops ~blk_alloc ~with_pcache ~flush_tl

  let _ = make
    
  (* let empty_pcache_state ~root_ptr ~current_ptr = Pcache.empty_pcache_state S2.empPcache_state.empty_pcache_state ~root_ptr ~current_ptr ~empty:empty_kvop_map *\) *)

(*
  module Pvt : sig 
    type pcache_descr = private pcache_ops
  end = struct
    type pcache_descr = pcache_ops
  end
*)
(*
  type pcache_descr = { 
    (* blk_alloc:(unit -> (r,t)m);  *)
    (* with_pcache:(pcache_state,t)with_state; *)
    (* flush_tl:pcache_state -> (unit,t)m; *)
    pcache_ops:pcache_ops
  }

  let find ~pd = pd.pcache_ops.find 
  let insert ~pd = pd.pcache_ops.insert
  let delete ~pd = pd.pcache_ops.delete
  let detach ~pd = pd.pcache_ops.detach ()
  let blk_len ~pd = pd.pcache_ops.blk_len
  let pcache_write ~pd = pd.pcache_ops.pcache_write
  let pcache_sync ~pd = pd.pcache_ops.pcache_sync ()
  let read_pcache ~pd = pd.pcache_ops.read_pcache
*)
end




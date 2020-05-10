(** Simplified, providing an object version *)


(* FIXME how is this much of an improvement on plain pc/make? *)

open Pcache_intf
open Pcache_intf.Pcache_state


module type S = sig
  type k
  type v
  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
  (* type r = Std_types.r[@@deriving bin_io,yojson] *)
  val k_cmp: k -> k -> int
end

module Make(S:S) = struct  

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
  
  module Pcache = Tjr_pcache.Make.Make(S2)
  include Pcache

  let make 
      ~(blk_alloc:(unit -> (r,t)m))
      ~(with_pcache:((_,_)pcache_state,t)with_state) 
      ~(flush_tl:(_,_)pcache_state -> (unit,t)m)
    : (_,_,_,_,_) pcache_ops = 
    let read_blk_as_buf _r = failwith "pc-ex/make_1: in this example, we don't read back" in
    Pcache.pcache_factory ~blk_alloc |> fun fact -> 
    fact#with_read_blk_as_buf ~read_blk_as_buf ~flush_tl |> fun fact -> 
    fact#make_pcache_ops#with_state with_pcache

  let _ = make
    
end




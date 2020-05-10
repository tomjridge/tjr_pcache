(** Pcache example, k and v are ints *)


module Int_int = struct
  include Std_types
  type k = int
  let k_mshlr = bp_mshlrs#int_mshlr
  type v = int
  let v_mshlr = bp_mshlrs#int_mshlr
  let r_mshlr = bp_mshlrs#r_mshlr  (* FIXME add to std_types *)

  let k_cmp = Int_.compare
end

module Pcache = Tjr_pcache.Make.Make(Int_int)

let pcache_factory = Pcache.pcache_factory

(*

type blk_id = Blk_id_as_int.blk_id

let make ~blk_alloc ~with_pcache ~flush_tl 
  : (_,_,_,_,_) pcache_ops 
  = Int_int_ex.make ~blk_alloc ~with_pcache ~flush_tl 
*)

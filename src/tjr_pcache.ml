(** Main documentation entry point for [tjr_pcache] *)

(**

{2 Introduction}

This library implements a persistent cache (pcache), i.e., an on-disk
cache. This should be used with [tjr_btree] to reduce the writes going
to the B-tree.

The main modules are included below.

*)

module Pcache_intf = Pcache_intf

module Persistent_list = Persistent_list

module Persistent_chunked_list = Persistent_chunked_list

module Detachable_chunked_list = Detachable_chunked_list

module Detachable_map = Detachable_map

open Pcache_intf

(** Provide the functionality in the most generic way *)
module Generic_make_functor = struct
  module type S = sig
    type k
    type v
    type ptr
    type t
    val monad_ops: t monad_ops

    val alloc:(unit -> (ptr, t) m)

    type pl_data  (* data stored in a pl node (ie a blk); typically a buffer, byte array, or string *)
    type pl_internal_state
    val with_pl:(pl_internal_state, t) with_state
    val pl_state_ops: (pl_data,ptr,pl_internal_state) Pl_types.pl_state_ops
    val write_node: (pl_internal_state -> (unit, t) m)

    type pcl_internal_state
    type e = (k,v)op  (* each elt is an op *)

    val pcl_state_ops: (pl_data,e,pcl_internal_state)Pcl_types.pcl_state_ops
    val with_pcl: (pcl_internal_state, t) with_state

    val with_dmap:((ptr, k, v) Dmap_types.dmap_state, t) with_state 
  end

  module Make(S:S) = struct
    open Detachable_map
    open S

    module Internal = struct
      let pl_ops =
        Persistent_list.make_persistent_list ~monad_ops ~pl_state_ops ~write_node ~with_pl ~alloc

      let _ = pl_ops

      let pcl_ops = 
        Persistent_chunked_list.make_pcl_ops ~monad_ops ~pl_ops ~pcl_state_ops ~with_pcl

      let _ : (e, ptr, t) Pcl_types.pcl_ops = pcl_ops


      let dmap_dcl_ops = make_dmap_dcl_ops ~monad_ops ~pcl_ops ~with_dmap

      let _ = dmap_dcl_ops

      let dmap_ops = convert_dcl_to_dmap ~monad_ops ~dmap_dcl_ops

      let _ : (k, v, ptr, t) Dmap_types.dmap_ops = dmap_ops
    end

    let dmap_ops = Internal.dmap_ops
  end
end


module type S = sig
  type k
  type v
  type ptr
  type t
  type blk

  val monad_ops: t monad_ops

  val alloc:(unit -> (ptr, t) m)
  val write_node: (blk -> (unit, t) m)

  type pl_data = blk (* data stored in a pl node (ie a blk); typically a buffer, byte array, or string *)

  type pl_internal_state
  val with_pl:(pl_internal_state, t) with_state

  (* val pl_state_ops: (pl_data,ptr,pl_internal_state) Pl_types.pl_state_ops *)

  type pcl_internal_state
  val with_pcl: (pcl_internal_state, t) with_state

  (* type e = (k,v)op  (\* each elt is an op *\) *)
  (* val pcl_state_ops: (pl_data,e,pcl_internal_state)Pcl_types.pcl_state_ops *)


  val with_dmap:((ptr, k, v) Dmap_types.dmap_state, t) with_state 
end

(** A version which uses simple implementations for pl_internal_state and pcl_internal_state *)

module Make(S:S) = struct
(*
  open Detachable_map
  open S

  module Internal = struct
    let pl_ops =
      Persistent_list.make_persistent_list ~monad_ops ~pl_state_ops ~write_node ~with_pl ~alloc

    let _ = pl_ops

    let pcl_ops = 
      Persistent_chunked_list.make_pcl_ops ~monad_ops ~pl_ops ~pcl_state_ops ~with_pcl

    let _ : (e, ptr, t) Pcl_types.pcl_ops = pcl_ops


    let dmap_dcl_ops = make_dmap_dcl_ops ~monad_ops ~pcl_ops ~with_dmap

    let _ = dmap_dcl_ops

    let dmap_ops = convert_dcl_to_dmap ~monad_ops ~dmap_dcl_ops

    let _ : (k, v, ptr, t) Dmap_types.dmap_ops = dmap_ops
  end

  let dmap_ops = Internal.dmap_ops
*)
end


(** {2 Internal, for testing} *)

module Simple_pl_and_pcl_implementations = Simple_pl_and_pcl_implementations

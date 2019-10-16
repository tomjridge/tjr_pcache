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

  (** pl_sync: Execute this before closing the persistent list/dmap FIXME don't need pl_sync now *)
  type ('k,'v,'ptr,'t) dmap_with_sync = {
    dmap_ops:('k,'v,'ptr,'t)Dmap_types.dmap_ops;
    pl_sync: (unit -> (unit,'t)m)
  }


  module type S = sig
    type k
    type v
    type ptr
    type t
    val monad_ops: t monad_ops

    (* val alloc:(unit -> (ptr, t) m) *)

    type pl_data  (* data stored in a pl node (ie a blk); typically a buffer, byte array, or string *)
    type pl_internal_state
    val pl_state_ops: (pl_data,ptr,pl_internal_state) Pl_types.pl_state_ops

    type pcl_internal_state
    type e = (k,v)kvop  (* each elt is an op *)

    val pcl_state_ops: (pl_data,e,pcl_internal_state)Pcl_types.pcl_state_ops
  end

  module Make(S:S) = struct
    open Detachable_map
    open S

    let make_dmap_ops ~alloc ~with_pl ~write_node ~with_pcl ~with_dmap = 
      let module Internal = struct
        let pl_ops =
          Persistent_list.make_persistent_list ~monad_ops ~pl_state_ops ~write_node ~with_pl ~alloc

        let _ = pl_ops

        let pcl_ops = 
          Persistent_chunked_list.make_pcl_ops ~monad_ops ~pl_ops ~pcl_state_ops ~with_pcl

        let _ : (e, ptr, t) Pcl_types.pcl_ops = pcl_ops


        (* NOTE make_dmap_dcl_ops requires pcl_ops to use (k,v)Ins_del_op.op *)
        let dmap_dcl_ops = make_dmap_dcl_ops ~monad_ops ~pcl_ops ~with_dmap

        let _ = dmap_dcl_ops

        let dmap_ops = convert_dcl_to_dmap ~monad_ops ~dmap_dcl_ops

        let _ : (k, v, ptr, t) Dmap_types.dmap_ops = dmap_ops
      end
      in
      let dmap_ops = Internal.dmap_ops in
      let pl_sync = Internal.pl_ops.pl_sync in
      {dmap_ops; pl_sync} 
  end
end


(*
module Make_functor = struct

  type pl_blk_with_next_ptr_and_index

  module type S = sig
    type k
    type v
    type ptr
    type t
    (* type blk *)

    val monad_ops: t monad_ops

    (* FIXME in simple pl_impl, we should refine data to be a blk-like thing with a next ptr at the front, and a current posn etc *)
    type pl_data = pl_blk_with_next_ptr_and_index 
    (* data stored in a pl node (ie a blk); typically a buffer, byte array, or string; FIXME do we need a ptr to the posn in the block? and store next pointer at front? *)

    (* FIXME this is more a runtime op *)
    val too_large: (k,v)Ins_del_op.op list -> bool
  end

  (** A version which uses simple implementations for pl_internal_state and pcl_internal_state *)
  module Make(S:S) = struct

    module I = Simple_pl_and_pcl_implementations

    (* aim to use generic_make_functor.make(S2) *)
    module S2 = struct
      include S
      type pl_internal_state = (pl_data,ptr) I.Pl_impl.pl_state
      let pl_state_ops = I.Pl_impl.pl_state_ops
      
      type e = (k,v)Ins_del_op.op
      type pcl_internal_state = e I.Pcl_impl.pcl_state

      let pcl_state_ops = I.Pcl_impl.make_pcl_state_ops ~too_large
    end

    (* FIXME need to complete this; prefer simple_pcl to use a buffer and pointer module G = Generic_make_functor.Make(S2) *)

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
end
*)


(** {2 Internal, for testing} *)

module Op_aux = Op_aux

module Simple_pl_and_pcl_implementations = Simple_pl_and_pcl_implementations


module Pcache_ctxts = Pcache_ctxts

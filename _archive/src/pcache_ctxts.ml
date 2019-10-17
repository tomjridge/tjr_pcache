open Pcache_intf

module Fixed = struct
  type buf
  type buf_pos = { buf:buf; pos:int }
  type pl_data = buf_pos
  type 'r pl_internal = { next: 'r option; buf:buf; pos:int }
  type ('k,'v) op = ('k,'v) kvop
end

module type C_rt = sig
  type t
  val monad_ops: t monad_ops
  type r
  val blk_alloc: unit -> (r,t) m
end

module type C_pl = sig
  open Pl_types
  include C_rt
  type pl_data (* the type of data managed by pl; probably a buffer *)
  type pl_internal (* the internal type maintained by pl *)
  type nonrec pl_state_ops = (pl_data,r,pl_internal) pl_state_ops
  type nonrec pl_ops = (pl_data,r,t)pl_ops
end

(* module type C_rt = C_rt with type pl_data = (buf * r option) *)

module type C_pcl = sig
  open Pcl_types
  (* include C_pl *)
  include C_rt
  type pl_data
  type nonrec inserted_type = r inserted_type
  type pcl_internal 
  type e (* pcl element type *)  
  type nonrec pcl_state_ops = (pl_data,e,pcl_internal) pcl_state_ops (* internal *)
  type nonrec pcl_ops = (e,r,t) pcl_ops
end

module type C_dcl = sig
  open Dcl_types
  include C_rt
  type op
  type abs
  type nonrec abs_ops = (op,abs)abs_ops
  type nonrec dcl_state = (r,abs)dcl_state                            
  type nonrec dcl_ops = (op,abs,r,t)dcl_ops
end

module type C_dmap = sig
  open Dmap_types
  include C_rt
  type k
  type v  
  type nonrec dmap_state = (r,k,v) dmap_state
  type nonrec dmap_dcl_ops = (r,k,v,t) dmap_dcl_ops
  type nonrec detach_info = (k,v,r) detach_info
  type nonrec dmap_ops = (k,v,r,t) dmap_ops
end


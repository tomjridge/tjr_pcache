(** A dmap is effectively just a DCL. However, we also include
   functionality to convert to a standard map interface (not one based
   on ops). *)

open Tjr_monad.Types
open Dcl_types
open Ins_del_op_type

(** NOTE dmap_state is just an abbreviation for dcl_state *)
type ('ptr,'k,'v) dmap_state = 
  ('ptr,
   ('k,('k,'v)op)Tjr_polymap.t) dcl_state
(*
= {
  start_block:'ptr;
  current_block:'ptr;
  block_list_length:int;
  abs_past:('k,('k,'v)op)Tjr_polymap.t;
  abs_current:('k,('k,'v)op)Tjr_polymap.t;
}
*)

(* just check types match up *)
let internal_ ~(ptr:'ptr) ~(abs:('k,('k,'v)op)Tjr_polymap.t) =
  let _x : ('ptr,'k,'v) dmap_state = {start_block=ptr;current_block=ptr; block_list_length=1;abs_past=abs; abs_current=abs} in
  let _y :  ('ptr,
             ('k,('k,'v)op)Tjr_polymap.t) dcl_state = _x in
  ()

(** NOTE dmap_ops is just an abbreviation for dcl_ops *)
type ('ptr,'k,'v,'t) dmap_ops = 
  (('k,'v) op, (* 'op *)
   ('k,('k,'v)op)Tjr_polymap.t,  (* 'abs *)
   'ptr,
   't) dcl_ops

type ('k,'v) dmap_as_map_detach_result = 
  (('k,('k,'v)op)Tjr_polymap.t * 
       ('k,('k,'v)op)Tjr_polymap.t)

(** For the detach operation, we get the map upto the current node,
   and the map for the current node *)
type ('k,'v,'t) dmap_as_map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t)m;
  detach: unit -> ( ('k,'v) dmap_as_map_detach_result, 't) m;
  block_list_length: unit -> (int,'t)m;
}



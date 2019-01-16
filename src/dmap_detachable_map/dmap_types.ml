(** A dmap is effectively just a DCL with a refined 'op type and 'abs
   type. However, we also include functionality to convert to a
   standard map interface (not one based on ops). *)

open Tjr_monad.Types
open Dcl_types
(* open Ins_del_op_type *)

type ('k,'v) op = ('k,'v) Ins_del_op_type.op

(** Abbreviation; FIXME move to Ins_del_op_type *)
type ('k,'v) op_map = ('k,('k,'v)op)Tjr_polymap.t

(** NOTE dmap_state is just an abbreviation for dcl_state *)
type ('ptr,'k,'v) dmap_state = 
  ('ptr,
   ('k,'v) op_map) dcl_state

(* just check types match up *)
let internal_ ~(ptr:'ptr) ~(abs:('k,('k,'v)op)Tjr_polymap.t) =
  let _x : ('ptr,'k,'v) dmap_state = {start_block=ptr;current_block=ptr; block_list_length=1;abs_past=abs; abs_current=abs} in
  let _y :  ('ptr,
             ('k,('k,'v)op)Tjr_polymap.t) dcl_state = _x in
  ()

(** NOTE dmap_dcl_ops is just an abbreviation for dcl_ops with:

- 'op the type of kv op
- 'abs the type of kv op_map

 *)
type ('ptr,'k,'v,'t) dmap_dcl_ops = 
  (('k,'v) op, 
   ('k,'v) op_map,
   'ptr,
   't) dcl_ops

(** The result of "detaching" the map. We get the abstract map for all
   but the current node, and information about the current node. *)
type ('k,'v,'ptr) detach_info = { 
  past_map: ('k,'v) op_map;
  current_map: ('k,'v) op_map;
  current_ptr: 'ptr
}

(** For the detach operation, we get the map upto the current node,
   and the map for the current node *)
type ('k,'v,'ptr,'t) dmap_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t)m;
  detach: unit -> ( ('k,'v,'ptr) detach_info, 't) m;
  block_list_length: unit -> (int,'t)m;
}



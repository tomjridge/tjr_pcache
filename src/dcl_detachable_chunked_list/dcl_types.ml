(** The DCL types *)

open Tjr_monad.Types

(* 
TODO

- need insert_many? 

*)

open Ins_del_op_type



(** The return type of the detach operation *)
type ('ptr,'map) detach_result = {
  old_ptr:'ptr;
  old_map:'map;
  new_ptr:'ptr;
  new_map:'map;
}


(* dcl ops ---------------------------------------------------------- *)

(** The DCL ops, [find], [add], [detach] and [block_list_length].

    [detach] indicates that we should start a new cache from the
   current block. The return result is the ptr and map corresponding
   to the contents of everything up to the current block, and the ptr
   and map for the current block. The intention is that the detached
   part is then rolled into the B-tree. If we only have 1 block, then
   nothing is rolled up. This occurs when [old_ptr] is the same as
   [new_ptr] FIXME use a new type *)
type ('k,'v,'map,'ptr,'t) dcl_ops = {
  find: 'k -> (('k,'v) op option,'t) m;  
  add: ('k,'v)op -> (unit,'t) m;  
  (* NOTE add rather than insert, to avoid confusion *)
  
  detach: unit -> (('ptr,'map)detach_result, 't) m;

  block_list_length: unit -> (int,'t) m;
}
(** NOTE [block_list_length]: this is the number of blocks from the
   underlying chunked list, used to store the ops (not the
   representation of the map!  since there may be two ops -- ins and
   del -- with the same key)

NOTE detach returns: 'ptr to first block in list; map upto current
   node; 'ptr to current node; map for current node *)


(* dcl state -------------------------------------------------------- *)

(** The state of the DCL. Parameters are:

- [start_block] is the root of the log
- [current_block] is the current block being written to
- [map_past] is the map from root to just before [current_block]
- [map_current] is the map for the current block

*)
type ('map,'ptr) dcl_state = {
  start_block: 'ptr;  
  current_block: 'ptr;
  block_list_length: int;
  map_past: 'map;  (* in reverse order *)
  map_current: 'map;
}

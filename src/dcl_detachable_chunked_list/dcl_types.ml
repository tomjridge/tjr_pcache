(** The DCL types *)

open Tjr_monad.Types

(* 
TODO

- need insert_many? 

*)


(* abs operations --------------------------------------------------- *)

(** The 'abs type is eg a map. The 'op type is eg insert(k,v), delete(k) *)
type ('op,'abs) abs_ops = {
  empty: 'abs;
  add: 'op -> 'abs -> 'abs;
  merge: 'abs -> 'abs -> 'abs;  (* second arg takes precedence *)
}

let abs_singleton ~abs_ops op = abs_ops.add op abs_ops.empty
  


(* dcl state -------------------------------------------------------- *)

(** The state of the DCL. Parameters are:

- [start_block] is the root of the log
- [current_block] is the current block being written to
- [abs_past] is the abstract view of ops from root to just before [current_block]
- [abs_current] is the abstract view of ops for the current block

NOTE unlike Pl and Pcl, we have a concrete type for the state, since
we don't expect to have any extra info stored at this point. But
perhaps we can avoid some of these extra type params if we keep dcl
state abstract as ['dcl_state]. But this seems unlikely.

NOTE [block_list_length]: this is the number of blocks from the
   underlying chunked list, used to store the ops (not the
   abstract representation!)
*)
type ('ptr,'abs) dcl_state = {
  start_block: 'ptr;  
  current_block: 'ptr;
  block_list_length: int;
  abs_past: 'abs;  
  abs_current: 'abs;
}



(* dcl ops ---------------------------------------------------------- *)

(** The DCL ops: [add], [peek], [detach] and [block_list_length].

    [detach] indicates that we should start a new cache from the
   current block. The return result is the ptr and map corresponding
   to the contents of everything up to the current block, and the ptr
   and map for the current block. The intention is that the detached
   part is then rolled into the B-tree. If we only have 1 block, then
   nothing is rolled up. This occurs when [old_ptr] is the same as
   [new_ptr] FIXME use a new type *)
type ('op,'abs,'ptr,'t) dcl_ops = {
  add: 'op -> (unit,'t) m;     (* NOTE add rather than insert, to avoid confusion *)
  peek: unit -> (('ptr,'abs)dcl_state,'t) m;    
  detach: unit -> (('ptr,'abs)dcl_state, 't) m;
  block_list_length: unit -> (int,'t) m;
}
(** NOTE detach returns: 'ptr to first block in list; map upto current
   node; 'ptr to current node; map for current node *)





(* old
(** The return type of the detach operation

NOTE the type 'abs stands for "abstract ops", ie some abstraction from
   the list of operations performed so far.

*)
type ('ptr,'abs) detach_result = {
  old_ptr:'ptr;
  old_abs:'abs;
  new_ptr:'ptr;
  new_abs:'abs;
}
*)


(** The DCL types *)

open Tjr_monad.Types

(* 
TODO

- need insert_many? 

*)


(* abs operations --------------------------------------------------- *)

(** Operations on the "abstract" state. The pcl state is something like a list of map operations. The 'abs type is something like the "map" view of these operations (needed because the list is redundant, or at the very least inefficient for map operations). Type vars:


- ['op] is eg insert(k,v), delete(k) 
- 'abs is the "abstraction"

Operations:

- empty, the empty map
- add, to add an operation to the abstract state
- merge, to merge two maps (second takes precedence); used when a new node is created, and the old node is merged into the accumulated past nodes.


*)
type ('op,'abs) abs_ops = {
  empty: 'abs;
  add: 'op -> 'abs -> 'abs;
  merge: 'abs -> 'abs -> 'abs;  (* second arg takes precedence *)
}

let abs_singleton ~abs_ops op = abs_ops.add op abs_ops.empty
  


(* dcl state -------------------------------------------------------- *)

(** The state of the DCL. Fields are:

- [start_block] is the root of the log
- [current_block] is the current block being written to
- [abs_past] is the abstract view of ops from root to just before [current_block]
- [abs_current] is the abstract view of ops for the current block

NOTE unlike Pl and Pcl, we have a concrete type for the state, since
we don't expect to have any extra info stored at this point. (FIXME what about dcl_dummy_implementation where we need to store all ptrs?) But
perhaps we can avoid some of these extra type params if we keep dcl
state abstract as ['dcl_state]. But this seems unlikely.

NOTE [block_list_length]: this is the number of blocks from the
   underlying chunked list, used to store the ops (not the
   abstract representation!)

NOTE upto this point, the pl and the pcl have not explicitly tracked the start of the list. FIXME perhaps they should? This has advantages in that the abstraction is self-contained.

*)
type ('ptr,'abs) dcl_state = {
  start_block: 'ptr;  
  current_block: 'ptr;
  block_list_length: int;
  abs_past: 'abs;  
  abs_current: 'abs;
}



(* dcl ops ---------------------------------------------------------- *)

(** The DCL ops: 

- [add] to add an op
- [peek] to reveal the dcl_state (FIXME why?)
- [detach] to issue a detach operation (eg prior to rolling the past entries into a B-tree)
- [block_list_length] to provide information to help determine when to roll up

The [detach] operation means that we should start a new cache from the
   current block. 

The return result is the ptr and map corresponding
to the contents of everything up to the current block, and the ptr
and map for the current block. The intention is that the detached
part is then rolled into the B-tree. If we only have 1 block, then
nothing is rolled up. This occurs when [old_ptr] is the same as
[new_ptr] FIXME use a new type 

NOTE detach returns the dcl_state since this includes at least all the fields we need.
*)
type ('op,'abs,'ptr,'t) dcl_ops = {
  add: 'op -> (unit,'t) m;     (* NOTE add rather than insert, to avoid confusion *)
  peek: unit -> (('ptr,'abs)dcl_state,'t) m;    
  detach: unit -> (('ptr,'abs)dcl_state, 't) m;
  block_list_length: unit -> (int,'t) m;
}




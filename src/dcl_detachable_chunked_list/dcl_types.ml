open Tjr_monad.Types

(* actions ---------------------------------------------------------- *)

(* we need a concrete representation of actions; these are the
   elements that get written to disk *)


(* FIXME perhaps to avoid marshalling issues and type vars we should
   work with bytes? *)

(* FIXME need insert_many *)

(** An op is either insert or delete. These are the entries that get
   written to disk. *)
type ('k,'v) op = ('k,'v) Pcl_test.op = Insert of 'k * 'v | Delete of 'k

(**/**)
let op2k = function
  | Insert (k,_v) -> k
  | Delete k -> k
(**/**)


(* we have to decide what information we need to keep for the "current
   chunk" *)

(* FIXME this doesn't work; why?
type ('k,'v,'repr) chunk_state = (('k,'v)op,'repr) pcl_state = {
  elts: (('k,'v) op) list;
  elts_repr: 'repr
}

*)


(* FIXME needed? type ('k,'v,'repr) chunk_state = (('k,'v)op,'repr) pcl_state *)


(** The type for the abstract view of the persistent cache. NOTE the
   values are ('k,'v)op, not 'v. *)
type ('k,'v,'map) kvop_map_ops = ('k,('k,'v)op,'map) Tjr_map.map_ops

(** The pcache ops, [find], [add], [detach] and
   [get_block_list_length]. 

    [detach] indicates that we should start a
   new cache from the current block. The return result is the ptr and
   map corresponding to the contents of everything up to the current
   block, and the ptr and map for the current block. The intention is
   that the detached part is then rolled into the B-tree. *)
type ('k,'v,'map,'ptr,'t) plog_ops = {
  find: 'k -> (('k,'v) op option,'t) m;  
  (* should execute in mem but to control concurrency we put in the
     monad FIXME? something better can be done? *)

  add: ('k,'v)op -> (unit,'t) m;  (* add rather than insert, to avoid confusion *)
  
  detach: unit -> ('ptr * 'map * 'ptr * 'map, 't) m;

  get_block_list_length: unit -> (int,'t) m;
}
(** NOTE detach returns: 'ptr to first block in list; map upto current node; 'ptr to current node; map for current node *)


(** The state of the persistent cache. Parameters are:

- [start_block] is the root of the log
- [current_block] is the current block being written to
- [map_past] is the map from root to just before [current_block]
- [map_current] is the map for the current block

*)
type ('map,'ptr) plog_state = {
  start_block: 'ptr;  
  current_block: 'ptr;
  block_list_length: int;
  map_past: 'map;  (* in reverse order *)
  map_current: 'map;
}

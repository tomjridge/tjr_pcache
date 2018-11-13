(** A persistent cache, used to reduce traffic via the B-tree. NOTE
   append-only logs are quickest when data must be stored persistently
   *)

(*

TODO:

- need an in-memory map of current and past operations

- need API functions to query the union of the current and past maps,
  and to get the past map as a list

- rename to persistent cache
*)


(*

Interactive:

#thread;;
#require "imp_fs";;

open Imp_fs;;

*)

open Tjr_map

(* open Tjr_monad.Monad *)
(* open Tjr_btree.Block *)
(* open Tjr_btree.Base_types  (\* mref *\) *)
open Tjr_monad.Types
open Tjr_monad.Mref

(* we construct on top of a persistent_chunked_list *)

module Pl = Persistent_list
module Pcl = Persistent_chunked_list
open Pcl

include Dcl_types

(**/**)
(** A map built from two other maps; prefer m2 *)
let map_find_union ~map_ops ~m1 ~m2 k = 
  let open Tjr_map in
  map_ops.map_find k m2 |> function
  | Some _ as x -> x
  | None -> 
    map_ops.map_find k m1
(**/**)

(* FIXME what about initialization? *)

(** Construct the persistent cache operations. Parameters:

- [monad_ops], the monadic operations
- [map_ops], the in-memory cache of (k -> (k,v)op) map
- [insert], the chunked list insert operation
- [plog_state_ref], the ref to the persistent log state
*)
let make_plog_ops
    ~monad_ops
    ~(map_ops:('k,'v,'map)kvop_map_ops)
    ~insert
    ~plog_state_ref
  : ('k,'v,'map,'ptr,'t) plog_ops 
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  let { map_find; map_add; map_empty; _ } = map_ops in
  let map_union m1 m2 = Tjr_map.map_union ~map_ops ~m1 ~m2 in
  (* NOTE get and set are for the plog_state component of the state *)
  let {get;set} = plog_state_ref in
  (* ASSUME start_block is initialized and consistent with pcl_state *)
  let find k : (('k,'v) op option,'t) m =
    get () >>= fun s ->
    let map_find = map_find_union ~map_ops ~m1:s.map_past ~m2:s.map_current in    
    let r = map_find k in
    return r
  in    
  let add op =
    get () >>= fun s ->
    insert op >>= function
    | Inserted_in_current_node ->
      get () >>= fun s' ->
      set { s' with map_current=map_ops.map_add (op2k op) op s'.map_current } 
    | Inserted_in_new_node ptr ->
      get () >>= fun s' ->
      set { s' with 
            current_block=ptr;
            block_list_length=s'.block_list_length+1;
            map_past=map_union s.map_past s.map_current;
            map_current=map_ops.map_add (op2k op) op map_empty }
  in
  (* FIXME be clear about concurrency here: detach happens in memory,
     almost instantly, but other operations cannot interleave with it
     even if it is in the monad FIXME perhaps we prefer a "single
     step" detach without the use of bind, which introduces
     non-atomicity *)
  let detach () =  
    get () >>= fun s ->
    let r = (s.start_block,s.map_past,s.current_block,s.map_current) in
    (* we need to adjust the start block and the map_past - we are
       forgetting everything in previous blocks *)
    set { s with start_block=s.current_block; block_list_length=1; map_past=map_empty } >>= fun () ->
    return r
  in
  let get_block_list_length () = get () >>= fun s ->
    return s.block_list_length
  in
  { find; add; detach; get_block_list_length }  


let _ = make_plog_ops


(* FIXME we probably want a common instantiation here, so we can get a
   pcache without constructing the intermediate layers explicitly *)
(** The abstract view of the pcache *)
let plog_to_map ~map_union s = map_union s.map_past s.map_current




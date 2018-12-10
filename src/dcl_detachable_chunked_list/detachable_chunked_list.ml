(** A persistent cache, used to reduce traffic via the B-tree. NOTE
   append-only logs are quickest when data must be stored persistently

    NOTE this code is not concurrent safe. Access must be serialized.

   *)

(*

TODO:

- need an in-memory map of current and past operations

- need API functions to query the union of the current and past maps,
  and to get the past map as a list?

- prefer with_dcl_state rather than get and set

*)


open Tjr_map

open Tjr_monad.Types
(* open Tjr_monad.Mref *)
open Tjr_monad.With_state
(* we construct on top of a persistent_chunked_list *)

open Ins_del_op_type

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
- [pcl_ops], the persistent chunked list ops
- [with_dcl], access the dcl state

*)
let make_dcl_ops
    ~monad_ops
    (* ~(kvop_map_ops:('k,'v,'map)kvop_map_ops) *)
    ~(pcl_ops:(('k,'v)op,'ptr,'t)pcl_ops)
    ~(with_dcl:('dcl_state,'t)with_state)
  : ('k,'v,'map,'ptr,'t) dcl_ops 
  =
  let kvop_map_ops = Ins_del_op_type.default_kvop_map_ops() in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  let { map_find; map_add; map_empty; _ } = kvop_map_ops in
  let map_union m1 m2 = Tjr_map.map_union ~map_ops:kvop_map_ops ~m1 ~m2 in
  let with_dcl = with_dcl.with_state in
  (* ASSUME start_block is initialized and consistent with pcl_state *)
  let find k : (('k,'v) op option,'t) m =
    with_dcl (fun ~state:s ~set_state -> 
        let map_find = 
          map_find_union 
            ~map_ops:kvop_map_ops 
            ~m1:s.map_past 
            ~m2:s.map_current 
        in    
        let r = map_find k in
        return r)
  in    
  let add op =
    with_dcl (fun ~state:s ~set_state -> 
        pcl_ops.insert op >>= function
        | Inserted_in_current_node ->
          set_state { s with 
                      map_current=
                        kvop_map_ops.map_add (op2k op) op s.map_current }
        | Inserted_in_new_node ptr ->
          (* NOTE this code isn't concurrent safe *)
          set_state { s with 
                      current_block=ptr;
                      block_list_length=s.block_list_length+1;
                      map_past=map_union s.map_past s.map_current;
                      map_current=kvop_map_ops.map_add (op2k op) op map_empty })
  in
  (* FIXME be clear about concurrency here: detach happens in memory,
     almost instantly, but other operations cannot interleave with it
     even if it is in the monad FIXME perhaps we prefer a "single
     step" detach without the use of bind, which introduces
     non-atomicity *)
  (* FIXME NOTE that even if the current block is full, detach should
     detach UPTO the current block, given that the dcl only knows the
     current block pointer *)
  let detach () =  
    with_dcl (fun ~state:s ~set_state -> 
        let r = {
          old_ptr=s.start_block;
          old_map=s.map_past;
          new_ptr=s.current_block;
          new_map=s.map_current}
        in
        (* we need to adjust the start block and the map_past - we are
           forgetting everything in previous blocks *)
        set_state { s with 
                    start_block=s.current_block; 
                    block_list_length=1;  (* NOTE current block may be
                                             empty, but still
                                             allocated *)
                    map_past=map_empty } >>= fun () ->
        return r)
  in
  let block_list_length () = 
    with_dcl (fun ~state ~set_state -> 
        return state.block_list_length)
  in
  { find; add; detach; block_list_length }  


let _ = make_dcl_ops


(* FIXME we probably want a common instantiation here, so we can get a
   pcache without constructing the intermediate layers explicitly *)
(** The abstract view of the pcache *)
let dcl_to_map ~map_union s = map_union s.map_past s.map_current

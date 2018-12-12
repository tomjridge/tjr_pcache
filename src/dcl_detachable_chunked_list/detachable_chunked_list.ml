(** A persistent cache, used to reduce traffic via the B-tree. NOTE
   append-only logs are quickest when data must be stored persistently

    NOTE this code is not concurrent safe. Access must be serialized.

*)

open Tjr_monad.Types
open Tjr_monad.With_state

open Pcl_types
include Dcl_types


(** Construct the dcl operations. Parameters:

- [monad_ops], the monadic operations
- [pcl_ops], the persistent chunked list ops
- [with_dcl], access the dcl state

*)
let make_dcl_ops
    ~monad_ops
    ~(pcl_ops:('op,'ptr,'t)pcl_ops)
    ~(with_dcl:('dcl_state,'t)with_state)
    ~(abs_ops:('op,'abs)abs_ops)
  : ('op,'abs,'ptr,'t) dcl_ops 
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  let with_dcl = with_dcl.with_state in
  (* ASSUME start_block is initialized and consistent with pcl_state *)
  let add op =
    with_dcl (fun ~state:s ~set_state -> 
        pcl_ops.insert op >>= function
        | Inserted_in_current_node ->
          set_state { s with abs_current=abs_ops.add op s.abs_current }
        | Inserted_in_new_node ptr ->
          (* NOTE this code isn't concurrent safe *)
          set_state { s with 
                      current_block=ptr;
                      block_list_length=s.block_list_length+1;
                      abs_past=abs_ops.merge s.abs_past s.abs_current;
                      abs_current=abs_singleton ~abs_ops op})
  in
  let peek () = 
    with_dcl (fun ~state:s ~set_state -> return s)
  in
  let detach () =  
    with_dcl (fun ~state:s ~set_state -> 
        (* we need to adjust the start block and the map_past - we are
           forgetting everything in previous blocks *)
        let new_state = { s with 
                          start_block=s.current_block; 
                          block_list_length=1;  (* NOTE current block may be
                                                   empty, but still
                                                   allocated *)
                          abs_past=abs_ops.empty }
        in
        set_state new_state >>= fun () -> 
        return s)
  in
  let block_list_length () = 
    with_dcl (fun ~state ~set_state -> 
        return state.block_list_length)
  in
  { add; peek; detach; block_list_length }  


let _ = make_dcl_ops


(*

(* FIXME we probably want a common instantiation here, so we can get a
   pcache without constructing the intermediate layers explicitly *)
(** The abstract view of the pcache *)
let dcl_to_map ~map_union s = map_union s.map_past s.map_current


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
  let { map_find; map_add; map_empty; _ } = kvop_map_ops in
  let map_union m1 m2 = Tjr_map.map_union ~map_ops:kvop_map_ops ~m1 ~m2 in


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



  (* FIXME be clear about concurrency here: detach happens in memory,
     almost instantly, but other operations cannot interleave with it
     even if it is in the monad FIXME perhaps we prefer a "single
     step" detach without the use of bind, which introduces
     non-atomicity *)
  (* FIXME NOTE that even if the current block is full, detach should
     detach UPTO the current block, given that the dcl only knows the
     current block pointer *)

*)

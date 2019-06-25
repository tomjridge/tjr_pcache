(** A "detachable list", with an operation [detach] to drop everything
   but the current node.

    NOTE this code is not concurrent safe. Access must be serialized.
   *)

open Pcache_intf
open Pcl_types
open Dcl_types


(** Construct the dcl operations. Parameters:

- [monad_ops], the monadic operations
- [pcl_ops], the persistent chunked list ops
- [with_dcl], access the dcl state
- [abs_ops], operations on the abstract data (see {!Detachable_map} for an example where the abstract data is a map)

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


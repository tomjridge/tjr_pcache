(** A dummy impl for the DCL, for testing.


NOTE the layered implementation is in {! Detachable_chunked_list }.
 *)

open Tjr_monad.Types
open Ins_del_op_type
open Dcl_types

(** NOTE for pointers, we make sure to allocate a new pointer every
   time we detach a non-empty prefix.

- kv_ops: a list of kv ops (must be a list not a map, since it represents the ops that are on disk)
- ptrs: all ptrs revealed so far
- ptr: points to the start of the block list

 *)

(* type ('k,'v,'ptr) dcl_dummy_state = {
 *   kv_ops: ('k,'v) op list;   
 *   ptrs: 'ptr list;
 *   ptr:'ptr; 
 * } *)

(* NOTE we have to keep track of pointers that have been made visible. *)
type ('k,'v,'ptr) dcl_dummy_state = {
  dcl_state:('ptr,('k,'v)op list) dcl_state;
  ptrs:'ptr list
}


(* let initial_dummy_state ~ptr = {
 *   kv_ops=[];
 *   ptrs=[ptr];
 *   ptr;
 * } *)

let initial_dummy_state ~ptr = {
  dcl_state = {
    start_block=ptr;
    current_block=ptr;
    block_list_length=1;
    abs_past=[];
    abs_current=[]
  };
  ptrs=[] }


let make_ops ~monad_ops ~ops_per_block ~new_ptr ~with_state =
  (* without the monad *)
  (* let find k t = List.assoc_opt k (List.map (fun op -> op2k op,op) t.kv_ops) in *)
  let add op t = { 
    t with dcl_state = {
      t.dcl_state with abs_current=op::t.dcl_state.abs_current }} 
  in
  let detach' t = 
    (* FIXME the problem here is that detach assumes a fixed number of
       ops per block *)
    let abs_current = t.dcl_state.abs_current in
    let n = List.length abs_current in
    let n_remaining = n mod ops_per_block in
    let remaining = Tjr_list.take n_remaining abs_current in
    let _dropped = Tjr_list.drop n_remaining abs_current in
    match n = n_remaining with
    | true -> 
      (* ie, we didn't drop any blocks *)
      (* let open Dcl_types in *)
      `Unchanged t
    | false -> 
      let ptr = new_ptr t.ptrs in
      let t' = { ptrs=ptr::t.ptrs; 
                 dcl_state={ 
                   start_block=ptr;
                   current_block=ptr;
                   block_list_length=1;
                   abs_past=[];
                   abs_current=remaining }} 
      in
      `Changed(t',t.dcl_state)  (* NOTE we return the old dcl_state *)
  in

  (* with the monad *)
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in      
  let with_state = with_state.with_state in
  (* let find k = with_state (fun ~state ~set_state ->
   *     return (find k state))
   * in *)
  let add op = with_state (fun ~state ~set_state ->
      set_state (add op state))
  in
  let peek () = with_state (fun ~state ~set_state ->
      return state.dcl_state)
  in
  let detach () = with_state (fun ~state ~set_state ->
      detach' state |> function
      | `Unchanged r -> return r.dcl_state
      | `Changed(s',r) -> 
        set_state s' >>= fun () ->
        return r)
  in
  let block_list_length () = with_state (fun ~state ~set_state ->
      (* When the list is empty, we still have 1 block allocated. When
         the first block fills, we don't allocate a new block till
         another kv pair is added. This gives the current correspondence (where n is the number of ops per block):

| # ops | # blocks
| 0     | 1
| n     | 1
| n+1   | 2
| n+n   | 2
| 2n+1  | 3

ie 1+ #ops/n

 *)
      let n_ops = List.length state.dcl_state.abs_past +
                  List.length state.dcl_state.abs_current in
      return (1+ (n_ops / ops_per_block)))
  in

  let open Dcl_types in
  {add;peek;detach;block_list_length}

let _ = make_ops

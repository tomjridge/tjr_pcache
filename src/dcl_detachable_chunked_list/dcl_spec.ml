(** A simple implementation of a DCL *)
open Tjr_monad.Types
open Ins_del_op_type
open Dcl_types

(** Contents of a block *)
type ('k,'v) blk = ('k,'v) op list

(** The spec state. [current] is stored in reverse order (most recent at front). *)
type ('k,'v,'ptr) dcl_spec_state = {
  ptrs: 'ptr list;  
  (** all pointers that have been used, including current_ptr; reverse
     order *)

  current: ('k,'v) blk;
  current_ptr: 'ptr;  (** ptr to current block *)

  undetached_ptr: 'ptr;
  undetached_blocks: ('k,'v) blk list;
}

let spec_to_assocl s = 
  s.current@(List.concat s.undetached_blocks) 
  |> List.map (fun op -> (op2k op,op))


type ('spec,'t) with_dcl_spec = {
  with_dcl_spec: 
    'a. 
      (spec:'spec -> 
       set_spec:('spec -> (unit,'t)m) -> 
       ('a,'t) m) 
    -> ('a,'t)m
}

let make_dcl_spec_ops ~monad_ops ~with_dcl_spec ~block_list_length ~new_ptr
  : ('k,'v,'map,'ptr,'t) dcl_ops
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  let with_dcl_spec = with_dcl_spec.with_dcl_spec in
  let find k = 
    with_dcl_spec (fun ~spec ~set_spec ->      
        let v = List.assoc_opt k (spec_to_assocl spec) in
        return v)
  in
  let add op = 
    with_dcl_spec (fun ~spec ~set_spec ->
        match List.length spec.current >= block_list_length with
        | true -> (
            let undetached_blocks = 
              spec.current::spec.undetached_blocks
            in
            let current = [op] in
            (* also need a new ptr *)
            let current_ptr = new_ptr spec.ptrs in
            let ptrs = current_ptr::spec.ptrs in           
            set_spec {spec with 
                      ptrs;
                      current;
                      current_ptr;                      
                      undetached_blocks })
        | false -> 
          let current = op::spec.current in
          set_spec {spec with current })
  in
  let detach () = 
    (* detach up to (not including) current block *)
    with_dcl_spec (fun ~spec ~set_spec ->
        let undetached_ptr = spec.current_ptr in
        let undetached_blocks = [] in
        set_spec {spec with undetached_blocks; undetached_ptr} >>= fun () ->
        return { 
          old_ptr=spec.undetached_ptr; 
          old_map=spec.undetached_blocks; 
          new_ptr=spec.current_ptr;
          new_map=[spec.current]})
  in
  let get_block_list_length = fun () -> return block_list_length in
  { find; add; detach; get_block_list_length }

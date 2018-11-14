(** An executable spec for the DCL. Note that [block_list_length] is
   not part of the spec, since it ties us to a fixed number of ops per
   block. However, in order to implement detach, we have to assume
   that there is a fixed number of ops per block.

    One thing that could be part of the spec is the size of the map
   domain, for maps current and past.  

    The alternative spec, in {! Dcl_spec}, just executably checks that
   detach does the right thing, which is more sensible.

    So really this should be considered a dummy implementation.

*)

(* NOTE this is used in tjr_kv.sync_store, for testing *)
open Tjr_monad.Types
open Ins_del_op_type


(** NOTE for pointers, we make sure to allocate a new pointer every
   time we detach a non-empty prefix *)
type ('k,'v,'ptr) state = {
  kvs: ('k,'v) op list;  (* association list, with dups *)
  ptrs: 'ptr list;
  ptr:'ptr;  (* points to the start of the blocks *)
}


type ('spec,'t) with_spec = {
  with_spec: 
    'a. 
      (spec:'spec -> 
       set_spec:('spec -> (unit,'t)m) -> 
       ('a,'t) m) 
    -> ('a,'t)m
}


let make_spec_ops ~monad_ops ~ops_per_block ~new_ptr ~with_spec =

  (* without the monad *)
  let kvs2assoc = fun kvs -> List.map (fun op -> op2k,op) kvs in
  let find k t = List.assoc_opt k (kvs2assoc t.kvs) in
  let add op t = { t with kvs=op::t.kvs } in
  let detach t = 
    (* FIXME the problem here is that detach assumes a fixed number of ops per block *)
    let n = List.length t.kvs in
    let n_remaining = n mod ops_per_block in
    let remaining = Tjr_list.take n_remaining t.kvs in
    let dropped = Tjr_list.drop n_remaining t.kvs in
    match n = n_remaining with
    | true -> 
      (* ie, we didn't drop any blocks *)
      let open Dcl_types in
      `Unchanged
        { old_ptr=t.ptr;old_map=[];new_ptr=t.ptr;new_map=t.kvs }
    | false -> 
      let ptr = new_ptr t.ptrs in
      let spec' = { t with ptrs=ptr::t.ptrs; ptr } in
      let open Dcl_types in
      let result = {
        old_ptr=t.ptr;
        old_map=dropped;
        new_ptr=ptr;
        new_map=remaining}
      in
      `Changed(spec',result)
  in

  (* with the monad *)
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in      
  let with_spec = with_spec.with_spec in
  let find k = with_spec (fun ~spec ~set_spec ->
      return (find k spec))
  in
  let add op = with_spec (fun ~spec ~set_spec ->
      set_spec (add op spec))
  in
  let detach () = with_spec (fun ~spec ~set_spec ->
      detach spec |> function
      | `Unchanged r -> return r
      | `Changed(spec,r) -> 
        set_spec spec >>= fun () ->
        return r)
  in
  let block_list_length () = with_spec (fun ~spec ~set_spec ->
      (* NOTE this should match the length returned by the dcl
         implementation, which in turn tracks the chunked list block
         allocation FIXME check this and put calculation here FIXME or
         perhaps this is just a performance-related metric, and isn't
         actually part of the spec, since this ties the to a fixed
         number of ops per block calculated by mod... but there is no
         reason to make this so precise. *)
      return (1+ (List.length spec.kvs / ops_per_block)))
  in
  let open Dcl_types in
  {find;add;detach;block_list_length}

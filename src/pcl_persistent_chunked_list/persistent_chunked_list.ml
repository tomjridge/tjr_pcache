(** Implement a "chunked" list (multiple items per node) using
   {!Persistent_list}. Automatically create a new node when the
   current fills up.

   We want to avoid repeated serialization. So the representation of
   the kv is present already as a separate type. We need a function:

   ['repr -> 'kv -> 'repr]

   which extends the 'repr type with another 'kv; we also need a way
   to check that 'repr fits in the block.

   An alternative is just to allocate a range of blocks contiguously,
   and write operations into these blocks consecutively. But this is a
   bit horrible.

NOTE not concurrent safe; access must be serialized.

*)

open Tjr_monad.Types
open Tjr_monad.With_state
open Pl_types


include Pcl_types

(** Function to construct a persistent chunked list. Parameters:
- [pl_ops] The underlying persistent list operations.
- [repr_ops] The marshalling functionality.
- [pcl_state_ref] The internal state of the pcl.
*)
let make_pcl_ops
    ~monad_ops
    ~pl_ops 
    ~(repr_ops:('e,'repr) repr_ops)
    ~with_pcl
    : ('e,'ptr,'t) pcl_ops
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let with_pcl = with_pcl.with_state in
  let { replace_last; new_node } = pl_ops in
  let { nil; snoc; _ } = repr_ops in
  let insert (e:'e) = 
    with_pcl (fun ~state:s ~set_state ->         
        let { elts=_; elts_repr } = s in
        snoc e elts_repr |> function
        | `Ok new_elts_repr -> 
          let s = { elts=s.elts@[e]; elts_repr = new_elts_repr } in
          set_state s >>= fun () ->
          (* we can write the new contents into the list *)
          replace_last new_elts_repr >>= fun () ->
          return Inserted_in_current_node
        | `Error_too_large ->
          (* we can't fit this new elt; so make a new node and try again *)
          snoc e nil |> function 
          | `Error_too_large -> 
            (* FIXME ASSUMES we need to be sure that any singleton list
               [elt] can fit in a Persistent_list node *)
            failwith __LOC__
          | `Ok new_elts_repr ->
            let s = { elts=[e]; elts_repr=new_elts_repr } in
            set_state s >>= fun () ->
            (* NOTE the following allocates a new node and updates the
               pointer in the old node *)
            new_node new_elts_repr >>= fun ptr ->
            return (Inserted_in_new_node ptr))
  in
  { insert }


let _ : 
  monad_ops:'t monad_ops ->
  pl_ops:('repr, 'ptr, 't) pl_ops -> 
  repr_ops:('e, 'repr) repr_ops -> 
  with_pcl:'with_pcl
  -> ('e,'ptr,'t) pcl_ops
  = 
  make_pcl_ops




(* debugging -------------------------------------------------------- *)

(* we use the plist debug code, but map usign repr_to_list *)
(** Abstract view of the persistent chunked list. *)
let pcl_to_nodes 
    ~(repr_ops:('e,'repr)repr_ops)
    ~read_node
    ~(ptr:'ptr) 
    ~blks
  : ('ptr * 'e list) list 
  =
  Persistent_list.plist_to_nodes ~read_node ~ptr ~blks
  |> List.map (fun (ptr,n) -> (ptr,n.contents |> repr_ops.repr_to_list))

let pcl_to_list
    ~(repr_ops:('e,'repr)repr_ops)
    ~(read_node:'ptr -> 'blks -> ('ptr, 'repr) pl_node)
    ~(ptr:'ptr) 
    ~(blks:'blks)
  : ('e list) list 
  =
  pcl_to_nodes ~repr_ops ~read_node ~ptr ~blks 
  |> List.map (fun (ptr,es) -> es)

let _ = pcl_to_list

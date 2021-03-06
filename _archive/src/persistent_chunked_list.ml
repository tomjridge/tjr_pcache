(** Implement a "chunked" list (multiple items per node) using
   {!Persistent_list}. Automatically create a new node when the
   current fills up.

NOTE not concurrent safe; access must be serialized.

*)


open Pcache_intf
open Pl_types
open Pcl_types
open Profilers

module Internal = struct
  [@@@warning "-8"]
  let [ins; ins'] = 
    ["ins";"ins'"] |> List.map intern

  let profiler = pcl_profiler
  let mark = pcl_profiler.mark
end
open Internal

(** Function to construct a persistent chunked list. Parameters:
- [pl_ops] The underlying persistent list operations.
- [pcl_state_ops, with_pcl] For the internal state of the pcl.

The returned ops:
- [insert], in the Inserted_in_current_node case, does not write to disk or update pl; in the new_node case, the underlying pl should issue disk writes
- [pcl_write/sync], will update pl and then call pl_write/sync

NOTE by default we do not call any lower level operations for the insert case.

*)
let make_pcl_ops
    ~(monad_ops:'t monad_ops)
    ~(pl_ops:('pl_data,'ptr,'t)pl_ops)
    ~(pcl_state_ops:('pl_data,'e,'i)pcl_state_ops)
    ~(with_pcl: ('i,'t)with_state)
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let with_pcl = with_pcl.with_state in
  let { replace_last; new_node; pl_write; _ } = pl_ops in
  let { nil;snoc;pl_data } = pcl_state_ops in
  let pcl_write () = 
    with_pcl (fun ~state:s ~set_state -> 
      pl_data  s |> fun data -> 
      replace_last data >>= fun () -> 
      pl_write ())
  in
  let pcl_sync () = pcl_write () in (* FIXME *)
  let insert (e:'e) = 
    return () >>= fun () -> 
    mark ins;
    with_pcl (fun ~state:s ~set_state ->         
        snoc s e |> function
        | `Ok s' -> 
          set_state s' >>= fun () ->
          return Inserted_in_current_node
        | `Error_too_large ->
          (* we can't fit this new elt; so make a new node and try again *)
          snoc (nil()) e |> function 
          | `Error_too_large -> 
            (* FIXME ASSUMES we need to be sure that any singleton list
               [elt] can fit in a Persistent_list node *)
            failwith __LOC__
          | `Ok s' ->
            (* NOTE the following allocates a new node and updates the
               pointer in the old node *)
            pcl_write() >>= fun () ->
            new_node (pl_data s') >>= fun ptr ->            
            set_state s' >>= fun () ->
            return (Inserted_in_new_node ptr))
      >>= fun r -> 
      mark ins';
      return r
  in
  { insert; pcl_write; pcl_sync }


(* NOTE how 'pl_data and 'i disappear in result *)
let _ : 
monad_ops:'t monad_ops ->
pl_ops:('pl_data, 'ptr, 't) pl_ops ->
pcl_state_ops:('pl_data, 'e, 'i) pcl_state_ops ->
with_pcl:('i, 't) with_state -> 
('e, 'ptr, 't) pcl_ops
=
  make_pcl_ops


(** This is just [pl_to_nodes] *)
let pcl_to_nodes = Persistent_list.pl_to_nodes

(** As [pcl_to_nodes] *)
let pcl_to_es_node_list
      ~monad_ops
      ~read_node
      ~ptr
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  pcl_to_nodes ~monad_ops ~read_node ~ptr >>= fun xs ->
  xs |> List.map (fun (ptr,(es,next)) -> (ptr,(es,next))) |> return


let _ = pcl_to_es_node_list


(** Drop pointers from [pcl_to_es_node_list] *)
let pcl_to_elt_list_list 
      ~monad_ops
      ~read_node 
      ~ptr
  : ('e list list,'t) m
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  pcl_to_es_node_list 
    ~monad_ops
    ~read_node 
    ~ptr
  >>= fun xs ->
  xs |> List.map (fun (ptr,(es,_)) -> es) |> return


let _
: monad_ops:'t monad_ops ->
read_node:('a -> ('e list * 'a option, 't) m) ->
ptr:'a -> ('e list list, 't) m
= pcl_to_elt_list_list


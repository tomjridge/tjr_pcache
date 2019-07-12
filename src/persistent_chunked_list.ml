(** Implement a "chunked" list (multiple items per node) using
   {!Persistent_list}. Automatically create a new node when the
   current fills up.

NOTE not concurrent safe; access must be serialized.

*)


open Pcache_intf
open Pl_types
open Pcl_types


module Profiler = Make_profiler()
open Profiler

(** Function to construct a persistent chunked list. Parameters:
- [pl_ops] The underlying persistent list operations.
- [pcl_state_ops, with_pcl] For the internal state of the pcl.
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
  let { replace_last; new_node; pl_sync } = pl_ops in
  let { nil;snoc;pl_data } = pcl_state_ops in
  let profile_m s m = 
    return () >>= fun () -> 
    mark s;
    m >>= fun r ->
    mark (s^"'");
    return r
  in

  let insert (e:'e) = 
    profile_m "pcl_insert" @@ with_pcl (fun ~state:s ~set_state ->         
        mark "ab";
        snoc s e |> function
        | `Ok s' -> 
          mark "ac";
          pl_data s' |> fun data ->
          mark "ad";          
          replace_last data >>= fun () ->
          mark "ae";
          set_state s' >>= fun () ->
          mark "bc";
          return Inserted_in_current_node
        | `Error_too_large ->
          mark "cd";
          (* we can't fit this new elt; so make a new node and try again *)
          snoc (nil()) e |> function 
          | `Error_too_large -> 
            (* FIXME ASSUMES we need to be sure that any singleton list
               [elt] can fit in a Persistent_list node *)
            failwith __LOC__
          | `Ok s' ->
            (* NOTE the following allocates a new node and updates the
               pointer in the old node *)
            new_node (pl_data s') >>= fun ptr ->            
            set_state s' >>= fun () ->
            mark "de";
            return (Inserted_in_new_node ptr))
  in
  { insert }


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
    ~(read_node:'ptr -> 'blks -> ('a list * 'ptr option))
    ~(ptr:'ptr)
    ~(blks:'blks)
  : ('ptr * ('e list * 'ptr option)) list 
  =
  pcl_to_nodes ~read_node ~ptr ~blks
  |> List.map (fun (ptr,(es,next)) -> (ptr,(es,next)))


let _ = pcl_to_es_node_list


(** Drop pointers from [pcl_to_es_node_list] *)
let pcl_to_elt_list_list 
  ~read_node 
  ~ptr
  ~blks
  : 'e list list
  =
  pcl_to_es_node_list 
    ~read_node 
    ~ptr
    ~blks
  |> List.map (fun (ptr,(es,_)) -> es)


let _ = pcl_to_elt_list_list

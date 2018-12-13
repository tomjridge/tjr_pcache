open Tjr_monad.Types
(* open Pl_types *)

(** Pure interface for manipulating the [pcl_state] *)
type ('pl_data,'e,'i) pcl_state_ops = {
  nil:unit -> 'i;
  snoc: 'i -> 'e -> [ `Error_too_large | `Ok of 'i ];
  pl_data:'i -> 'pl_data
}


(* pcl_ops ---------------------------------------------------------- *)

(** A type that records whether an element was inserted in the current
   node, or whether a new node was allocated to hold the element. *)
type 'ptr inserted_type = 
    Inserted_in_current_node | Inserted_in_new_node of 'ptr

(** The interface exposed by the persistent chunked list, a single
   [insert] function. *)
type ('e,'ptr,'t) pcl_ops = {
  insert:'e -> ('ptr inserted_type,'t) m
}



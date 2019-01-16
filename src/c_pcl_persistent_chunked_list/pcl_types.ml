open Tjr_monad.Types
(* open Pl_types *)

(** Pure interface for manipulating the [pcl_state]. Type vars:

- ['i] the internal pcl state type (kept abstract)
- ['e] the non-marshalled element type
- ['pl_data] the type of data stored in a persistent list node (there is also a pointer in the pl node)


Functions:

- [nil] the empty state corresponding to a new node created when the old node is full; use pl_data to get the underlying pl_node
- [snoc] to add an element
- [pl_data] to project from the pcl state to the actual data to be written


NOTE that the "next" pointer manipulation has been confined to the persistent list interface.

 *)
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
   [insert] function. NOTE how 'pl_data and 'i have disappeared. *)
type ('e,'ptr,'t) pcl_ops = {
  insert:'e -> ('ptr inserted_type,'t) m
}



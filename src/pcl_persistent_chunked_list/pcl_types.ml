open Tjr_monad.Types
(* open Pl_types *)


(* pcl_state -------------------------------------------------------- *)

(* state we maintain; this is for the current chunk *)
(** The persistent chunked list (pcl) state. 

NOTE that ['repr] is the type for the representation of a list of elements.
*)
type ('e,'repr) pcl_state = {
  elts: 'e list;
  elts_repr: 'repr
}


(* marshalling ------------------------------------------------------ *)

(* what we need from marshalling *)

(** Routines for marshalling elements to disk. ['repr] is the
   underlying representation type; ['e] is the element type. Note that
   [snoc] (join element at end of list) takes an element and the {e
   representation} of the current node.  *)
type ('e,'repr) repr_ops = {
  nil: 'repr;
  snoc: 'e -> 'repr -> [ `Ok of 'repr | `Error_too_large ];  
  (* may not be able to snoc an element if it won't fit in the node *)

  repr_to_list: 'repr -> 'e list  (* inverse of marshalling *)
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

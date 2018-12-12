open Tjr_monad.Types

(* pure interface *)

(** The persistent list state. Each node consists of data and a
   possible next pointer (initially None, but may be set
   subsequently). For [new_node], the ptr is the ptr of the new block,
   and the second argument is the data. 

    NOTE the type ['a] is the type of the data stored in each node.

*)
type ('a,'ptr,'i) pl_state_ops = {
  set_data: 'a -> 'i -> 'i;
  set_next: 'ptr -> 'i -> 'i;
  (* get_data: 'i -> 'a; *)
  new_node: 'ptr -> 'a -> 'i -> 'i;  (* ptr is typically stored as the "current" ptr in 'i *)
}


(* FIXME rename plist *)

(** The operations provided by the persistent list. [replace_last]
   replaces the contents of the last element of the list. [new_node]
   allocates a new node at the end of the list and makes it the
   "current" node. *)
type ('a,'ptr,'t) pl_ops = {
  replace_last: 'a -> (unit,'t) m;
  new_node: 'a -> ('ptr,'t) m;  (* NOTE we return the ptr to the new node *)
}



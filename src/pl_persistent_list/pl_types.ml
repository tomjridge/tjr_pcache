open Tjr_monad.Types

(** The type of persistent list nodes with value['a] for each node; a
   singly-linked list. 

Nodes in the list have an optional next pointer, and contents.
*)
type ('ptr,'a) pl_node = {
  next: 'ptr option;
  contents: 'a;
}

(* the cursor state is in memory; make sure to write the current_node
   to disk; NOTE 'a is the type of the contents of the node

*)

(** The persistent list state. Consists of [current_ptr], a pointer to
   a block that is currently being written, and [current_node], the
   abstract representation of the contents of the node. *)
type ('ptr,'a) pl_state (* cursor_state *) = {
  current_ptr: 'ptr;  (* block we are currently updating *)
  current_node: ('ptr,'a) pl_node;  (* stored in mem to avoid rereading when moving to new node FIXME? *)
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



(* A persistent list of values, implemented as a persistent
   singly-linked list.

NOTE not concurrent safe; access must be serialized.
 *)

open Tjr_monad.Types
open Tjr_monad.Mref
include Pl_types

(** Make a persistent list. Parameters:
- [write_node] Write a node to disk, given a [ptr] to the block and the [list_node]
- [alloc] Allocate a new block
*)
(* FIXME couldn't get @param to work *)
let make_persistent_list 
    ~monad_ops
    ~(write_node : 'ptr -> ('ptr,'a) list_node -> (unit,'t) m) 
    ~(plist_state_ref : (('ptr,'a) plist_state,'t) mref)
    ~(alloc : unit -> ('ptr,'t) m)
  : ('a,'ptr,'t) list_ops
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let read_state,write_state = plist_state_ref.get, plist_state_ref.set in
  let replace_last contents = 
    read_state () >>= fun s ->
    let current_node = { s.current_node with contents } in
    write_node s.current_ptr current_node >>= fun () ->
    write_state { s with current_node } >>= fun () -> 
    return ()
  in
  let new_node contents = 
    alloc () >>= fun new_ptr ->
    read_state () >>= fun { current_ptr; current_node } ->
    (* write the current block with a valid next ptr *)
    let next = Some new_ptr in
    {current_node with next } |> write_node current_ptr >>= fun () ->
    (* construct new node *)
    { next=None; contents } |> fun new_node ->
    write_node new_ptr new_node >>= fun () ->
    (* NOTE this cannot be concurrent safe, given that it discards the
       "current" state and just writes a completely new state *)
    { current_ptr=new_ptr; current_node=new_node } |> fun s ->
    write_state s >>= fun () ->
    return new_ptr
  in
  { replace_last; new_node }


let _ = make_persistent_list

(** The abstract view of the persistent list. Parameters:
- [ptr] The root of the list.
*)
let plist_to_nodes 
    ~(read_node:'ptr -> 't -> ('ptr,'a)list_node) 
    ~(ptr:'ptr)
    s 
  : ('ptr * ('ptr,'a)list_node) list 
  (* NOTE in the return type, the first ptr is the pointer to the
     node; the snd is the optional link in the list_node *)
  =
  let rec loop ptr = 
    read_node ptr s |> fun node ->
    match node.next with 
    | None -> [(ptr,node)]
    | Some ptr' -> (ptr,node)::(loop ptr')
  in
  loop ptr

let _ = plist_to_nodes

(** The abstract view of the persistent list, without any pointer info. *)
let plist_to_list ~read_node ~ptr s = 
  plist_to_nodes ~read_node ~ptr s |> List.map (fun (_,n) -> n.contents)


(*
let rec plist_to_list ~read_node ~ptr =
  let acc = ref [] in
  let rec loop ptr = 
    read_node ptr >>= fun { next; contents } ->
    acc:=contents::!acc;
    match next with
    | None -> return (List.rev !acc)
    | Some ptr -> loop ptr
  in
  loop ptr



let _ = plist_to_list
*)



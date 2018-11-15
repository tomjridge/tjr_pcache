(* A persistent list of values, implemented as a persistent
   singly-linked list.

NOTE not concurrent safe; access must be serialized.
 *)

open Tjr_monad.Types
(* open Tjr_monad.Mref *)
open Tjr_monad.With_state
include Pl_types

(** Make a persistent list. Parameters:

- [write_node] Write a node to disk, given a [ptr] to the block and the [list_node]; this is an operation provided by the lower layer, and may take a long time to complete; assumed synchronous ie doesn't return till data is on disk

- [alloc] Allocate a new block; likely provided by pl, in conjunction with a sysytem free list

*)
(* FIXME couldn't get @param to work *)
let make_persistent_list 
    ~monad_ops
    ~(write_node : 'ptr -> ('ptr,'a) pl_node -> (unit,'t) m) 
    ~(alloc : unit -> ('ptr,'t) m)  (* could/should be part of
                                       plist_state? not necessarily*)
    ~(with_pl : (('ptr,'a) pl_state,'t) with_state) 
  : ('a,'ptr,'t) pl_ops
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let with_pl = with_pl.with_state in
  let replace_last contents = 
    (* FIXME we want this to complete without reading the node - so
       maintain a possible-next pointer *)
    with_pl (fun ~state:s ~set_state ->
        let current_node = { next=s.current_node.next; contents } in
        write_node s.current_ptr current_node >>= fun () ->        
        set_state { s with current_node })
  in
  let new_node contents = 
    alloc () >>= fun new_ptr ->
    with_pl (fun ~state:s ~set_state ->
        (* What if next is already set? FIXME maybe allow
           pre-allocation of next *)        

        (* write current node *)
        {s.current_node with next=Some new_ptr } 
        |> write_node s.current_ptr >>= fun () ->

        (* construct new node *)
        { next=None; contents } |> fun new_node ->
        write_node new_ptr new_node >>= fun () ->
        { current_ptr=new_ptr; current_node=new_node } |> fun s ->
        set_state s >>= fun () ->
        return new_ptr)
  in
  { replace_last; new_node }


let _ = make_persistent_list

(** The abstract view of the persistent list. Parameters:
- [ptr] The root of the list.


NOTE in the return type, the first ptr is the pointer to the
node; the snd is the optional link in the list_node

*)
let plist_to_nodes 
    ~(read_node:'ptr -> 'blks -> ('ptr,'contents)pl_node) 
    ~(ptr:'ptr)
    ~(blks:'blks)
  : ('ptr * ('ptr,'contents)pl_node) list 

  =
  let rec loop ptr = 
    read_node ptr blks |> fun node ->
    match node.next with 
    | None -> [(ptr,node)]
    | Some ptr' -> (ptr,node)::(loop ptr')
  in
  loop ptr


(** The abstract view of the persistent list, without any pointer info. *)
let plist_to_list ~read_node ~ptr ~blks : 'contents list = 
  plist_to_nodes ~read_node ~ptr ~blks |> List.map (fun (_,n) -> n.contents)

let _ = plist_to_list

    

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



(** Tests for persistent list *)

open Tjr_store
open Fstore_passing

open Simple_pl_and_pcl_implementations

module Make(S:sig 
    type data
    type ptr = int 

    val ptr0: ptr
    val initial_store: Tjr_store.fstore
    val data0: data (* initial contents of first node *)
end) = struct
  open S

  (* let ptr0 = 0 *)

  (* The internal state *)
  type pl_state = (data, ptr) Pl_impl.pl_state
  (* The on-disk data *)
  type pl_node = (data,ptr) Pl_impl.pl_node


  
  let next_free_ptr p = p+1

  (* ptr0 will be used to store the init node *)
  let free = next_free_ptr ptr0 

  let store = initial_store

  let with_free,store =
    mk_ref free store |> fun (free_ref,s) ->
    let with_free = Fstore_passing.fstore_ref_to_with_state free_ref in
    let with_free = with_free.with_state in
    with_free,s

  let alloc () = with_free (fun ~state:free ~set_state -> 
      let free' = next_free_ptr free in
      set_state free' >>= fun () ->
      return free)

  let _ = alloc
  
  let pl_state : pl_state = Pl_impl.{ data=data0; current=ptr0; next=None } 

  let _ : (data, ptr) Pl_impl.pl_state = pl_state

  let with_pl,store = 
    mk_ref pl_state store |> fun (pl_ref,s) ->
    let with_pl = Fstore_passing.fstore_ref_to_with_state pl_ref in
    (* let with_pl = with_pl.with_state in *)
    with_pl,s

  let _ : (pl_state, fstore_passing) with_state = with_pl
  
  let init_node : pl_node = Pl_impl.{ contents=data0; next=None }

  (* a map from ptr to pl_node *)
  module Blks = Map.Make(struct type t = ptr let compare = Pervasives.compare end)
  (* let map_ops : (ptr,pl_node,_)Tjr_map.map_ops = Tjr_map.make_map_ops Pervasives.compare *)

  let blks : pl_node Blks.t = Blks.empty |> Blks.add ptr0 init_node 

  let blks_ref,with_blks,store = 
    mk_ref blks store |> fun (blks_ref,s) ->
    let with_blks = Fstore_passing.fstore_ref_to_with_state blks_ref in
    let with_blks = with_blks.with_state in
    blks_ref,with_blks,s

  let _ = with_blks

  let write_node (pl_state:pl_state) = 
    with_blks (fun ~state:blks ~set_state -> 
      let node = Pl_impl.{next=pl_state.next; contents=pl_state.data} in
      set_state Pl_impl.(Blks.add pl_state.current node blks))

  let read_node ptr blks : data * ptr option = 
    Blks.find ptr blks |> fun Pl_impl.{ contents; next } -> contents,next

  let pl_state_ops = Pl_impl.pl_state_ops

  let pl_ops = Persistent_list.make_persistent_list
      ~monad_ops
      ~pl_state_ops
      ~write_node
      ~with_pl
      ~alloc

  let _ = pl_ops
end

(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let ptr0 = 0 in
  let module M = Make(struct 
      type data=string 
      type ptr=int 
      let initial_store=Tjr_store.empty_fstore ~allow_reset:true ()
      let ptr0=ptr0
      let data0="Start"
    end)
  in
  let ops = M.pl_ops in
  let cmds = 
    ops.replace_last "New start" >>= fun () ->
    ops.new_node "second node" >>= fun _ ->
    ops.new_node "third node" >>= fun _ ->
    ops.replace_last "alternative third node" >>= fun () ->
    ops.pl_write()
  in
  let init_state = M.store in
  let run ~init_state m = State_passing.to_fun m init_state in
  run ~init_state  cmds |> fun ((),s) ->
  (* check what is written *)
  let xs = Persistent_list.pl_to_list 
             ~read_node:M.read_node
             ~ptr:ptr0
             ~blks:(Tjr_store.get M.blks_ref s)
  in
  Sexplib.(Std.(Sexp.to_string_hum 
                  [%message "Debug, xs: " (xs: string list)])) |> print_endline;
  Alcotest.(check (list string)) "same strings" xs  ["New start";"second node";"alternative third node"];
  Printf.printf "%s: ...tests finished\n" __MODULE__;
  () (* actually return the state? *)

let test_set = [
  "Pl test", `Quick, main
]


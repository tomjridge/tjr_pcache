(** Tests for persistent list *)

open Tjr_monad.With_state
(* open Tjr_pcache *)

open Tjr_store
open Store_passing

module Blks = Tjr_polymap

(* a basic implementation of pl_state and pl_node ------------------- *)

open Pl_simple_implementation

let make_pl_test ~store ~data0 ~ptr0 ~next_free_ptr =
  mk_ref ptr0 store |> fun (s,free_ref) ->
  let with_free f = with_ref free_ref f in

  let alloc () = with_free (fun ~state:free ~set_state -> 
      let free' = next_free_ptr free in
      set_state free' >>= fun () ->
      return free)
  in
  
  let pl_state = { data=data0; current=ptr0; next=None } in
  mk_ref pl_state s |> fun (s,pl_ref) ->
  let with_pl f = with_ref pl_ref f in

  
  let init_node = { contents=data0; next=None } in
  let blks = Blks.empty Pervasives.compare |> Blks.add ptr0 init_node in
  mk_ref blks s |> fun (s,blks_ref) ->
  let with_blks f = with_ref blks_ref f in

  let _ = with_blks in

  let write_node = 
    Pl_simple_implementation.Write_node.write_node { with_state=with_blks} in

  let _ = write_node in

  let _read_node = Pl_simple_implementation.Write_node.read_node in

  (free_ref,pl_ref,blks_ref),s,Persistent_list.make_persistent_list 
    ~monad_ops 
    ~pl_state_ops 
    ~write_node 
    ~with_pl:{with_state=with_pl} 
    ~alloc

let _ = make_pl_test


(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let ptr0 = 0 in
  let (_,_,blks_ref),s,ops = make_pl_test
      ~store:Tjr_store.initial_store
      ~data0:"Start"
      ~ptr0
      ~next_free_ptr:(fun x -> x+1)
  in
  let cmds = 
    ops.replace_last "New start" >>= fun () ->
    ops.new_node "second node" >>= fun _ ->
    ops.new_node "third node" >>= fun _ ->
    ops.replace_last "alternative third node" 
  in
  let init_state = s in
  Tjr_monad.State_passing.run ~init_state  cmds |> fun ((),s) ->
  (* check what is written *)
  let xs = Persistent_list.pl_to_list 
      ~read_node:Pl_simple_implementation.Write_node.read_node 
      ~ptr:ptr0
      ~blks:(Tjr_store.get blks_ref s)
  in
  assert(xs = ["New start";"second node";"alternative third node"]);
  xs |> Tjr_string.concat_strings ~sep:";" |> fun str ->
  print_endline str;
  Printf.printf "%s: ...tests finished\n" __MODULE__;
  s  (* actually return the state? *)


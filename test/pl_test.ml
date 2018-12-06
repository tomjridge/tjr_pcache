(** Tests for persistent list

We work with a generic typed store {! Tjr_lib.Tjr_store}, so that we
   can later extend the state with further fields

*)

open Tjr_monad.Types
open Tjr_monad.State_passing
(* open Tjr_monad.Mref *)
open Tjr_monad.With_state
open Tjr_pcache
open Persistent_list

open Test_store
module Blks = Tjr_polymap
  

module Make(S: sig

    type ptr = int
    type node_contents

    val init_contents : node_contents
end) = struct

  open S

  (** The state of the whole system (? should be part of sys?);
     node_contents is the type of node contents *)
  type state = Tjr_store.t


  let monad_ops : state state_passing monad_ops = 
    Tjr_monad.State_passing.monad_ops'

  let ( >>= ) = monad_ops.bind 
  let return = monad_ops.return


  let init_node = {next=None;contents=init_contents}

  type blks = (ptr,(ptr,node_contents)pl_node) Blks.t
  (* NOTE we need to ensure that the blks contain a binding at least for the current_ptr as in pl_ref below *)
  let blks_ref =
    let blks = Blks.empty Pervasives.compare |> Blks.add 0 init_node in
    mk_ref' blks

  (* model the free list via an incrementing counter *)
  type free = ptr
  let free_ref = mk_ref' 1 

  let pl_ref = 
    mk_ref'
      { current_ptr=0; 
        current_node=init_node } 


  let with_ref r f = Tjr_monad.State_passing.with_state
      ~get:(fun x -> get r x) 
      ~set:(fun s t -> set r s t)
      ~f

  let with_blks f = with_ref blks_ref f

  let with_free f = with_ref free_ref f

  let with_pl f = with_ref pl_ref f



  (* write_node ------------------------------------------------------ *)

  let write_node ptr n = with_blks (fun ~state:blks ~set_state -> 
      set_state (Blks.add ptr n blks ))


  (* read_node (for abstraction) -------------------------------------- *)

  let read_node ptr blks = Blks.find ptr blks


  (* alloc ------------------------------------------------------------ *)

  let alloc () = with_free (fun ~state:free ~set_state -> 
      let free = free+1 in
      set_state free >>= fun () ->
      return (free-1))

  let _ = alloc

  let pl_ops = 
    make_persistent_list 
      ~monad_ops
      ~write_node 
      ~alloc 
      ~with_pl:{with_state=with_pl}

end

module A = Make(struct 
    type ptr = int 
    type node_contents=string 
    let init_contents = "Start"
end)
open A

(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  let ops = make_persistent_list 
      ~monad_ops
      ~write_node 
      ~alloc 
      ~with_pl:{with_state=with_pl}
  in
  let cmds = 
    ops.replace_last "New start" >>= fun () ->
    ops.new_node "second node" >>= fun _ ->
    ops.new_node "third node" >>= fun _ ->
    ops.replace_last "alternative third node" >>= fun () ->
    with_blks (fun ~state:blks ~set_state ->
        return (plist_to_list ~read_node ~ptr:0 ~blks))
  in
  let init_state = !test_store in
  Tjr_monad.State_passing.run ~init_state  cmds |> fun (xs,s) ->
  assert(xs = ["New start";"second node";"alternative third node"]);
  xs |> Tjr_string.concat_strings ~sep:";" |> fun str ->
  print_endline str;
  Printf.printf "%s: ...tests finished\n" __FILE__;
  s  (* actually return the state *)

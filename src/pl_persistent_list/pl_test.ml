(** Tests for persistent list *)

open Tjr_monad.Types
open Tjr_monad.State_passing
(* open Tjr_monad.Mref *)
open Tjr_monad.With_state
open Persistent_list

module Blks = Tjr_polymap

(** The state of the whole system (? should be part of sys?); 'a is the type
   of node contents *)
type ('ptr,'a,'pcl_state) state = {
  blks: ('ptr,('ptr,'a)pl_node) Blks.t; 
  free: 'ptr;  (* iso to ptr *)
  pl_state: ('ptr,'a) pl_state;
  pcl_state: 'pcl_state;
}
 
let monad_ops : ('ptr,'a,'pcl_state) state state_passing monad_ops = 
  Tjr_monad.State_passing.monad_ops'

let ( >>= ) = monad_ops.bind 
let return = monad_ops.return


let with_state f = Tjr_monad.State_passing.with_state
    ~get:(fun x -> x) 
    ~set:(fun s t -> s)
    ~f

let _ = with_state


let with_pl f = Tjr_monad.State_passing.with_state
    ~get:(fun x -> x.pl_state) 
    ~set:(fun s t -> {t with pl_state=s})
    ~f

let _ = with_pl



(* write_node ------------------------------------------------------ *)

let write_node ptr n = with_state (fun ~state:s ~set_state -> 
    set_state { s with blks=Blks.add ptr n s.blks })


(* read_node (for abstraction) -------------------------------------- *)

let read_node ptr blks = Blks.find ptr blks


(* alloc ------------------------------------------------------------ *)

let alloc () = with_state (fun ~state:s ~set_state -> 
    let free = s.free+1 in
    set_state { s with free } >>= fun () ->
    return (free-1))

let _ = alloc

(* FIXME note eta expansion; can we avoid? *)
let pl_ops () = 
  make_persistent_list 
    ~monad_ops 
    ~write_node 
    ~alloc 
    ~with_pl:{with_state=with_pl}

let _ = pl_ops  (* FIXME why existential tyvar?  *)

let start_block = 0

let init_state = 
  let root = start_block in
  let current_node={ next=None; contents="Start" } in
  {    
    blks=Blks.add root current_node (Blks.empty Pervasives.compare);
    pl_state={ current_ptr=root; current_node };
    free=root+1;
    pcl_state=()
  }


(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  let get_state () = with_world (fun s -> (s,s)) in
  let ops = pl_ops () in
  let cmds = 
    ops.replace_last "New start" >>= fun () ->
    ops.new_node "second node" >>= fun _ ->
    ops.new_node "third node" >>= fun _ ->
    ops.replace_last "alternative third node" >>= fun () ->
    get_state () >>= fun s ->
    return (plist_to_list ~read_node ~ptr:0 s.blks)
  in
  Tjr_monad.State_passing.run ~init_state cmds |> fun (xs,s) ->
  assert(xs = ["New start";"second node";"alternative third node"]);
  xs |> Tjr_string.concat_strings ~sep:";" |> fun str ->
  print_endline str;
  Printf.printf "%s: ...tests finished\n" __FILE__;
  s  (* actually return the state *)

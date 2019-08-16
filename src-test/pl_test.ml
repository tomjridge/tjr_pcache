(** Tests for persistent list *)
open Simple_pl_and_pcl_implementations

module Blk_id = Blk_id_as_int

module Map_ = Tjr_map.With_pervasives_compare

(** We construct an in-mem blk_dev, and the pcache layers on top *)
type ('a,'r) state = {
  pl_state: ('a,'r)Pl_impl.pl_state;
  min_free_blk: 'r;
  nodes: ('r,('a,'r)Pl_impl.pl_node)Map_.map_with_pervasives_compare;
}

let init_state = 
  let current = Blk_id.of_int 0 in
  Pl_impl.{ 
    pl_state={data="Start"; current; next=None};
    min_free_blk=Blk_id.of_int 1;
    nodes=Map_.empty ()
  }

open Tjr_monad.State_passing
let monad_ops = monad_ops ()
let ( >>= ) = monad_ops.bind
let return = monad_ops.return


let pl_state_ops = Pl_impl.pl_state_ops

let alloc () = of_fun (fun s -> 
    (s.min_free_blk,{s with min_free_blk=Blk_id.incr s.min_free_blk}))

let get_pl () = of_fun (fun s -> s.pl_state,s)

let set_pl pl_state = of_fun (fun s -> (),{s with pl_state})

let with_pl 
      (f: (
          state:'s -> 
          set_state:('s -> (unit,'t)m) -> 
          ('a,'t) m))
  : ('a,'t) m 
  = 
  get_pl () >>= fun state ->
  f ~state ~set_state:(fun s -> set_pl s)

let with_pl = {with_state=with_pl}

(*
let f_update f k v = fun k' -> 
  if k=k' then v else f k'
*)

let write_node =
  let open Pl_impl in
  fun (pl_state:('a,'r)Pl_impl.pl_state) ->
    let node = Pl_impl.{next=pl_state.next; contents=pl_state.data } in
    of_fun (fun (s:('a,'r)state) -> ((),{s with nodes=(Map_.add pl_state.current node s.nodes)}))
    
let pl = Persistent_list.make_persistent_list ~monad_ops ~pl_state_ops ~with_pl ~alloc ~write_node

let read_node r = of_fun (fun s ->
    Map_.find r s.nodes |> fun pl_node ->
    (Pl_impl.(pl_node.contents,pl_node.next),s))

(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let ops = pl in
  let cmds = 
    ops.replace_last "New start" >>= fun () ->
    ops.new_node "second node" >>= fun _ ->
    ops.new_node "third node" >>= fun _ ->
    ops.replace_last "alternative third node" >>= fun () ->
    ops.pl_write()
  in
  let run ~init_state m = State_passing.to_fun m init_state in
  run ~init_state cmds |> fun ((),s) ->
  (* check what is written *)
  let xs = Persistent_list.pl_to_list 
      ~monad_ops
      ~read_node
      ~ptr:(Blk_id.of_int 0)
  in
  run ~init_state:s xs |> fun (xs,_) -> 
  Sexplib.(Std.(Sexp.to_string_hum 
                  [%message "Debug, xs: " (xs: string list)])) |> print_endline;
  Alcotest.(check (list string)) "same strings" 
    xs  ["New start";"second node";"alternative third node"];
  Printf.printf "%s: ...tests finished\n" __MODULE__;
  () (* actually return the state? *)

let test_set = [
  "Pl test", `Quick, main
]


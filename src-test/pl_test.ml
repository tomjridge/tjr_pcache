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


(** Package up the test layers *)
(*
type ('a,'r,'pl_state) pl_layers = {
  monad_ops    : ('a, 'r) state state_passing monad_ops;
  init_state   :('a,'r)state;
  pl_state_ops :('a, 'r, 'pl_state) Pl_types.pl_state_ops;
  alloc        : unit -> ('r, ('a, 'r) state state_passing) m;
  with_pl      :('pl_state, ('a, 'r) state state_passing) with_state;
  write_node   :'pl_state -> (unit, ('a, 'r) state state_passing) m;
  read_node    :'r -> ('a * 'r option, ('a, 'r) state state_passing) m;
  pl_ops       :('a, 'r, ('a, 'r) state state_passing) Pl_types.pl_ops
}
*)

type ('a,'r,'pl_state,'t) pl_layers = {
  monad_ops    : 't monad_ops;
  init_state   :('a,'r)state;
  pl_state_ops :('a, 'r, 'pl_state) Pl_types.pl_state_ops;
  alloc        : unit -> ('r, 't) m;
  with_pl      :('pl_state, 't) with_state;
  write_node   :'pl_state -> (unit, 't) m;
  read_node    :'r -> ('a * 'r option, 't) m;
  pl_ops       :('a, 'r, 't) Pl_types.pl_ops
}


module Make(S:sig 
    type data
    (* type t *)
  end) = struct 
  open S

  open Tjr_monad.State_passing

  (* the monad depends on the type data; hence the rather circuitous
     construction which allows us to keep the data type abstract *)
  let monad_ops : (data,Blk_id.blk_id) state state_passing monad_ops = monad_ops ()
  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  let make_pl_layers =
    let module A = struct

      let pl_state_ops = Pl_impl.pl_state_ops

      let alloc () = of_fun (fun s -> 
          (s.min_free_blk,{s with min_free_blk=Blk_id.incr s.min_free_blk}))

      let get_pl () = of_fun (fun s -> s.pl_state,s)

      let set_pl pl_state = of_fun (fun s -> (),{s with pl_state})

      let _ = set_pl

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

      let _ = with_pl

      let write_node =
        let open Pl_impl in
        fun (pl_state:('a,'r)Pl_impl.pl_state) ->
          let node = Pl_impl.{next=pl_state.next; contents=pl_state.data } in
          of_fun (fun (s:('a,'r)state) -> ((),{s with nodes=(Map_.add pl_state.current node s.nodes)}))

      let _ = write_node

      let pl_ops = Persistent_list.make_persistent_list ~monad_ops ~pl_state_ops ~with_pl ~alloc ~write_node

      let _ = pl_ops

      let read_node (r:'r) = of_fun (fun s ->
          Map_.find r s.nodes |> fun pl_node ->
          (Pl_impl.(pl_node.contents,pl_node.next),s))

      let _ = read_node

      let pl_layers ~(init_state:('a,'r)state) = {
        monad_ops; init_state; pl_state_ops; alloc; with_pl;
        write_node; read_node; pl_ops }
    end
    in
    fun init_state -> A.pl_layers ~init_state
end

module Made = Make(struct type data=string end)

let init_state = 
  let current = Blk_id.of_int 0 in
  Pl_impl.{ 
    pl_state={data="Start"; current; next=None};
    min_free_blk=Blk_id.of_int 1;
    nodes=Map_.empty ()
  }

let pl_layers 
: (string, Blk_id.blk_id, (string, Blk_id.blk_id) Pl_impl.pl_state,
 (string, Blk_id.blk_id) state state_passing)
pl_layers
  = Made.make_pl_layers init_state

let _ = pl_layers


(* Write some new nodes, update some, and finally print out the list *)
let main () = 
  let { monad_ops; pl_ops; read_node; _ } = pl_layers in
  let ( >>= ) = monad_ops.bind in
  (* let return = monad_ops.return in *)
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let cmds = 
    pl_ops.replace_last "New start" >>= fun () ->
    pl_ops.new_node "second node" >>= fun _ ->
    pl_ops.new_node "third node" >>= fun _ ->
    pl_ops.replace_last "alternative third node" >>= fun () ->
    pl_ops.pl_write()
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


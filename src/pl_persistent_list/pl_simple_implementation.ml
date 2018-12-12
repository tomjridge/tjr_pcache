(** A simple implementation, for testing *)

type ('a,'ptr) pl_state = {
  data: 'a;
  current: 'ptr;
  next: 'ptr option
}

let pl_state_ops = Pl_types.{
    set_data=(fun data pl_state -> {pl_state with data});
    set_next=(fun ptr pl_state -> {pl_state with next=(Some ptr)});
    new_node=(fun current data pl_state -> { data; current; next=None })
  }  


type ('a,'ptr) pl_node = { 
  next:'ptr option; 
  contents:'a}


(** A simple impl of write node, using store passing with a ref to a
   'ptr -> 'a map *)
module Write_node = struct
  (* open Tjr_monad.Types *)
  open Tjr_monad.With_state

  let write_node with_blks (pl_state:('a,'ptr)pl_state) =
    let with_blks = with_blks.with_state in
    with_blks (fun ~state:blks ~set_state -> 
        let pl_node = { next=pl_state.next; contents=pl_state.data } in
        set_state (Tjr_polymap.add pl_state.current pl_node blks))

  let _ = write_node

  let read_node ptr blks =
    Tjr_polymap.find ptr blks |> fun pl_node ->
    (pl_node.contents, pl_node.next)
end

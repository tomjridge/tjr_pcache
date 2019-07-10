open Pcache_intf

(** A simple persistent list implementation, for testing *)
module Pl_impl = struct

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
    contents:'a
  }


end


(*
(** A simple impl of write node, using store passing with a ref to a
    'ptr -> 'a map *)
module Write_node_impl = struct
  open Pl_impl
  let write_node with_blks (pl_state:('a,'ptr)pl_state) =
    let with_blks = with_blks.with_state in
    with_blks (fun ~state:blks ~set_state -> 
        let pl_node = { next=pl_state.next; contents=pl_state.data } in
        set_state (Tjr_map.add pl_state.current pl_node blks))

  let _ = write_node

  let read_node ptr blks =
    Tjr_polymap.find ptr blks |> fun pl_node ->
    (pl_node.contents, pl_node.next)
end
*)

(** A simple implementation of a persistent chunked list *)
module Pcl_impl = struct

  type 'e pcl_state = {
    es:'e list

  }

  (** The too_large argument indicates when the list is too large (ie
     a new blk needs to be allocated *)
  let make_pcl_state_ops ~too_large = Pcl_types.{
      nil=(fun () -> {es=[]});
      snoc=(fun i e -> 
          let es' = i.es@[e] in
          match too_large es' with
          | true -> `Error_too_large
          | false -> `Ok {es=es'});
      pl_data=(fun i -> i.es);
    }

  let _ : too_large:('a list -> bool) ->
('a list, 'a, 'a pcl_state) Pcl_types.pcl_state_ops
    = make_pcl_state_ops
end

(** A persistent-on-disk list *)


open Pcache_intf
open Pcache_intf.Pl_types

module Pl_profiler = With_array.Make_profiler()
open Pl_profiler

let [pl_last; pl_last'; pl_sync; pl_sync'; new_node_i; new_node'] = List.map Pl_profiler.allocate_int ["pl_last";"pl_last'";"pl_sync";"pl_sync'"; "new_node"; "new_node'"]
[@@warning "-8"]

let make_persistent_list 
    ~monad_ops
    ~(pl_state_ops:('a,'ptr,'i) pl_state_ops)
    ~(write_node:'i -> (unit,'t) m)
    ~(with_pl: ('i,'t) with_state)
    ~alloc
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let {set_data;set_next;new_node} = pl_state_ops in
  let with_pl = with_pl.with_state in
  (* let profile_m = (Util.profile_m ~monad_ops ~mark) in *)
  (* let _ = profile_m in *)
  let replace_last (a:'a) =
    mark pl_last;
    with_pl (fun ~state:s ~set_state ->
        set_data a s |> fun s' ->
        (* write_node s' >>= fun () -> don't write on every change *)
        set_state s') >>= fun () -> 
    mark pl_last'; return ()
  in
  let pl_sync () = 
    mark pl_sync;
    with_pl (fun ~state:s ~set_state ->
      write_node s) >>= fun () -> 
    mark pl_sync'; return ()
  in
  let new_node (a:'a) = 
    mark new_node_i;
    alloc () >>= fun new_ptr ->
    with_pl (fun ~state:s ~set_state ->
        (* What if next is already set? FIXME maybe allow
           pre-allocation of next *)
        (* update current node and write *)
        s |> set_next new_ptr |> fun s' ->
        let update_old_node_with_ptr_to_new_node = write_node s' in
        new_node new_ptr a s' |> fun s' ->
        write_node s' >>= fun () ->
        update_old_node_with_ptr_to_new_node >>= fun () ->
        set_state s' >>= fun () ->
        return new_ptr)
    >>= fun ptr -> 
    mark new_node';
    return ptr
  in
  Pl_types.{ replace_last; new_node; pl_sync }

(* NOTE how the impl type 'i disappears in the following, except for
   write_node *)
let _ :
monad_ops:'t monad_ops ->
pl_state_ops:('a, 'ptr, 'i) pl_state_ops ->
write_node:('i -> (unit, 't) m) ->
with_pl:('i, 't) with_state ->
alloc:(unit -> ('ptr, 't) m) -> 
('a, 'ptr, 't) pl_ops
= make_persistent_list



(** Unmarshal a persistent list to a list of nodes. *)
let pl_to_nodes
  ~(read_node:'ptr -> 'blks -> ('a * 'ptr option))
  ~(ptr:'ptr)
  ~(blks:'blks)
  : ('ptr * ('a * 'ptr option)) list 
  =
  let rec loop ptr = 
    read_node ptr blks |> fun (a,next) ->
    match next with 
    | None -> [(ptr,(a,next))]
    | Some ptr' -> 
      assert(not(ptr' = ptr));  (* basic sanity check *)
      (ptr,(a,next))::(loop ptr')
  in
  loop ptr


(** Convenience to unmarshal to a list of node contents *)
let pl_to_list ~read_node ~(ptr:'ptr) ~(blks:'blks) =
  pl_to_nodes ~read_node ~ptr ~blks 
  |> List.map (fun (_,(a,_)) -> (a:'a))

let _ = pl_to_list

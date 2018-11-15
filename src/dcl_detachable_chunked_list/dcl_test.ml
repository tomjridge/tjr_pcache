(*
(** Test all the layers, pl, pcl and dcl.

This code uses [Dcl_dbg] to construct a "debug" or spec state for an
   [int -> int] map.

Then the 3 layers are constructed, and wrapped with code to check (via
   dcl_dbg) that the abstract view tracks the spec (this is in
   [checked_dcl]).

*)

open Tjr_monad
open Tjr_monad.Types
open Tjr_monad.State_passing

open Ins_del_op_type
open Pcl_types
open Dcl_types
open Dcl_dbg

(* state type for state passing ------------------------------------ *)

include struct
  type ptr = int
  open Pl_types
  open Pcl_test.Repr

  type ('k,'v) list_node = (ptr,('k,'v)repr) Pl_types.pl_node  

  type ('k,'v,'map,'dbg) state = {
    map: (ptr * ('k,'v)list_node) list;  (* association list *)
    free: ptr;  (* iso to ptr *)

    plist_state: (int,('k,'v)repr) plist_state;
    pclist_state: (('k,'v)op,('k,'v)repr) pcl_state;
    dcl_state: ('map,ptr) dcl_state;
    dbg: 'dbg; (* debug state *)
  }
end


(* monad ops -------------------------------------------------------- *)

let monad_ops : ('k,'v,'map,'dbg) state state_passing monad_ops = 
  Tjr_monad.State_passing.monad_ops ()


(* persistent list -------------------------------------------------- *)

(* NOTE FIXME copied from pcl *)
let list_ops () = 
  Persistent_list.make_persistent_list
    ~monad_ops
    ~write_node:(fun ptr node -> with_world (fun s -> ((),{ s with map=(ptr,node)::s.map })))
    ~plist_state_ref:{
      get=(fun () -> with_world (fun s -> (s.plist_state,s)));
      set=(fun plist_state -> with_world (fun s -> ((),{s with plist_state})))
    }
    ~alloc:(fun () -> with_world (fun s -> (s.free,{ s with free=s.free+1 })))

let _ = list_ops


(* chunked list ----------------------------------------------------- *)

(* this should ensure no more than 2 items in each block FIXME but
   it seems that this is not enforced BUG *)
let repr_ops = Pcl_test.Repr.repr_ops 2 (* FIXME parameterize tests by this *)

let chunked_list () =
  Persistent_chunked_list.make_persistent_chunked_list
    ~monad_ops
    ~list_ops:(list_ops ())
    ~repr_ops
    ~pcl_state_ref:{
      get=(fun () -> with_world (fun s -> (s.pclist_state,s)));
      set=(fun pclist_state -> with_world (fun s -> ((),{s with pclist_state})));
    }

let _ = chunked_list


(* DCL -------------------------------------------------------------- *)


(* extract the dcl part of the state *)
let with_dcl f = 
  State_passing.with_state
    ~get:(fun t -> t.dcl_state)
    ~set:(fun dcl_state t -> { t with dcl_state})
    ~f

let with_dcl = {
  with_state=with_dcl
}

let _ = with_dcl

let dcl ~map_ops = 
  chunked_list () |> fun { insert } -> 
  Detachable_chunked_list.make_dcl_ops
    ~monad_ops
    ~map_ops
    ~insert
    ~with_dcl

let _ : 
  map_ops:('k, ('k, 'v) op, 'map) Tjr_map.map_ops -> 
  ('k, 'v, 'map, ptr, ('k, 'v, 'map,'dbg) state state_passing) dcl_ops 
  = dcl



(* test with int -> int --------------------------------------------- *)

(* fix types of 'k 'v and 'map *)
(* test with an int -> int map *)

open Tjr_map

module Map_ = Tjr_map.Make(Int_ord)

let map_ops = Map_.map_ops

let dcl () = dcl ~map_ops

let _ : unit ->
  (ptr, 'a, (ptr, 'a) op Map_int.t, ptr,
   (ptr, 'a, (ptr, 'a) op Map_int.t,'dbg) state state_passing)
    dcl_ops
  = 
  dcl

let checked_dcl () : ('k,'v,'map,'ptr,'t) dcl_ops = 
  let read_node ptr s = List.assoc ptr s.map in
  let plist_to_nodes ~(ptr:ptr) (s:('k,'v,'map,'dbg)state) = 
    Persistent_list.plist_to_nodes ~read_node ~ptr s 
  in
  let repr_to_list = repr_ops.repr_to_list in
  let pclist_to_nodes ~ptr s = 
    Persistent_chunked_list.pclist_to_nodes ~repr_to_list ~plist_to_nodes ~ptr s
  in
  let _ = pclist_to_nodes in
  let dcl_to_dbg s = 
    dcl_to_dbg
      ~pclist_to_nodes
      ~get_dcl_state:(fun s -> s.dcl_state)
      s
  in
  let dcl_ops = dcl () in
  let set_dbg = fun dbg s -> {s with dbg} in
  let get_dbg = fun s -> s.dbg in
  make_checked_dcl_ops
    ~monad_ops
    ~dcl_ops
    ~dcl_to_dbg
    ~set_dbg
    ~get_dbg

let _ : (int,'v,'map,ptr,(int,'v,'map,'dbg)state state_passing)dcl_ops = 
  checked_dcl ()


(* testing ------------------------------------------------------ *)

let init_state = 
  let start_block = Pl_test.start_block in
  let i = Pcl_test.init_state ~repr_ops in
  {
    map=i.map;
    free=i.free;
    plist_state=i.plist_state;
    pclist_state=i.pclist_state;
    dcl_state={
      start_block;
      current_block=start_block;
      block_list_length=1;
      map_past=map_ops.map_empty;
      map_current=map_ops.map_empty
    };
    dbg=init_dbg
  }
[@@ocaml.warning "-40"]


(* FIXME use exhaustive testing? no need to wrap in checked_dcl *)
let test ~depth = 
  let num_tests = ref 0 in
  let dcl_ops = checked_dcl () in
  (* let dcl_ops = dcl () in *)
  (* the operations are: find k; add op; detach 

     given some finite range for k, we want to attempt each
     operation in each state; we are not too bothered about
     repeating work at this point *)
  let ks = [1;2;3] in
  let ops = 
    `Detach :: 
    (ks |> List.map (fun k -> [`Find(k);`Insert(k,2*k);`Delete(k)]) |> List.concat) 
  in
  (* we exhaustively test these operations up to a maximum depth;
     the test state is a decreasing count paired with the system
     state *)
  let run = Tjr_monad.State_passing.run in
  let rec step (count,s) =
    let f op = 
      num_tests:=!num_tests+1;
      match op with
      | `Detach -> 
        Pcache_debug.log "detach";
        run ~init_state:s (dcl_ops.detach ()) |> fun (_,s') -> s'
      | `Delete k -> 
        Printf.sprintf "delete(%d)" k |> Pcache_debug.log;
        run ~init_state:s (dcl_ops.add (Delete(k))) |> fun (_,s') -> s'
      | `Find k ->
        Printf.sprintf "find(%d)" k |> Pcache_debug.log;
        run ~init_state:s (dcl_ops.find k) |> fun (_,s') -> s'
      | `Insert(k,v) -> 
        Printf.sprintf "insert(%d,%d)" k v |> Pcache_debug.log;
        run ~init_state:s (dcl_ops.add (Insert(k,v))) |> fun (_,s') -> s'
    in
    let f op = 
      f op |> fun s' ->        
      Pcache_debug.log_lazy (fun () ->
          Printf.sprintf "%s: %s"
            "test, post op"
            (dbg_to_yojson s'.dbg |> Yojson.Safe.pretty_to_string));
      step (count-1,s')
    in
    if count <= 0 then () else ops |> List.iter f
  in
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  step (depth,init_state);
  Printf.printf "%s: ...tests finished\n%!" __FILE__;
  Printf.printf "%s: %d tests executed in total\n%!" __FILE__ !num_tests
[@@ocaml.warning "-8"]



*)

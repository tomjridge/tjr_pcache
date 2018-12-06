(** Test all the layers, pl, pcl and dcl.

This code uses [Dcl_dbg] to construct a "debug" or spec state for an
   [int -> int] map.

Then the 3 layers are constructed, and wrapped with code to check (via
   dcl_dbg) that the abstract view tracks the spec (this is in
   [checked_dcl]).

See notes in ../TESTING.org

*)

open Tjr_monad.Types
open Tjr_monad.State_passing

open Tjr_pcache
open Ins_del_op_type
open Pcl_types
open Dcl_types
open Dcl_dbg
open Pcl_test.Repr
open Test_store

module Logger = Tjr_fs_shared.Logger


module Make(S:sig 
    type ptr=int 
    type k = int
    type v = int
    val repr_ops: ((k,v)op, (k,v)repr) repr_ops
end) = struct
  open S

  (* pcl instance --------------------------------------------------- *)

  module A = struct
    type ptr = S.ptr
    type k = S.k
    type v = S.v
    let repr_ops = repr_ops
  end
  module Pcl_test' = Pcl_test.Make(A)
  open Pcl_test'.Pl_test'
  open Pcl_test'

  type map = (k,(k,v)op) Tjr_polymap.t      
  let empty_map : map = Tjr_polymap.empty Pervasives.compare
  let map_ops = 
    let open Tjr_polymap in
    let open Tjr_map in
    { map_empty=empty (Pervasives.compare);
      map_is_empty=is_empty;
      map_add=add;
      map_remove=remove;
      map_find=find_opt;
      map_bindings=bindings}

  type 'dbg dcl_state' = {
    dcl_state: (map,ptr) dcl_state;
    dbg: 'dbg; (* debug state *)
  }


  (* dcl_ref -------------------------------------------------------- *)

  let dcl_ref = mk_ref' {
      start_block=0;
      current_block=0;
      block_list_length=1;
      map_past=empty_map;
      map_current=empty_map
    } 

  let with_dcl f = with_ref dcl_ref f


  let dcl_ops = 
    Detachable_chunked_list.make_dcl_ops
      ~monad_ops
      ~map_ops
      ~pcl_ops
      ~with_dcl:{with_state=with_dcl}

  let _ :
    (k, v, map, ptr, state state_passing) dcl_ops
    = dcl_ops


  (* wrap dcl in checking code -------------------------------------- *)

  let store_to_dbg store =
    let blks = get blks_ref store in
    let dcl = get dcl_ref store in
    let pcl_to_list = 
      fun ~start_block ~blks ->
        Persistent_chunked_list.pcl_to_list
          ~repr_ops ~read_node ~ptr:start_block ~blks
    in
    dcl_to_dbg ~pcl_to_list ~blks ~dcl
      

  let checked_dcl_ops : ('k,'v,'map,'ptr,'t) dcl_ops = 
    Dcl_dbg.make_checked_dcl_ops
      ~monad_ops
      ~dcl_ops
      ~get_dbg:(fun () -> 
          Tjr_monad.State_passing.with_world 
            (fun s -> (s |> store_to_dbg,s)))
      
  let _ : 
    (int, int, (int, (int, int) op) Tjr_polymap.t, int, state state_passing)
      dcl_ops
    =
    checked_dcl_ops

end


(* testing ------------------------------------------------------ *)

module A = struct
  type ptr=int 
  type k = int
  type v = int
  let repr_ops = Pcl_test.Repr.make_repr_ops 2
end
module Dcl_test' = Make(A)


open Dcl_test'.Pcl_test'.Pl_test'
open Dcl_test'.Pcl_test'
open Dcl_test'


let _ = 
  Printf.printf "Free: %d\n%!" (!Test_store.test_store).free;
  Printf.printf "Refs: %d %d %d %d %d\n%!"
    (blks_ref |> Tjr_store.Refs.to_int)
    (free_ref |> Tjr_store.Refs.to_int)
    (pl_ref |> Tjr_store.Refs.to_int)
    (pcl_ref |> Tjr_store.Refs.to_int)
    (dcl_ref |> Tjr_store.Refs.to_int)

(* FIXME use exhaustive testing? no need to wrap in checked_dcl *)
let test ~depth = 
  let num_tests = ref 0 in
  let dcl_ops = checked_dcl_ops in
  (* the operations are: find k; add op; detach 

     given some finite range for k, we want to attempt each
     operation in each state; we are not too bothered about
     repeating work at this point *)
  let ks = [1;2;3] in
  let ops = 
    `Detach :: 
    (ks |> List.map (fun k -> [`Find(k);`Insert(k,2*k);`Delete(k)]) 
     |> List.concat) 
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
        Logger.log "detach";
        run ~init_state:s (dcl_ops.detach ()) |> fun (_,s') -> s'
      | `Delete k -> 
        Printf.sprintf "delete(%d)" k |> Logger.log;
        run ~init_state:s (dcl_ops.add (Delete(k))) |> fun (_,s') -> s'
      | `Find k ->
        Printf.sprintf "find(%d)" k |> Logger.log;
        run ~init_state:s (dcl_ops.find k) |> fun (_,s') -> s'
      | `Insert(k,v) -> 
        Printf.sprintf "insert(%d,%d)" k v |> Logger.log;
        run ~init_state:s (dcl_ops.add (Insert(k,v))) |> fun (_,s') -> s'
    in
    let f op = 
      f op |> fun s' ->        
      Logger.logl (fun () ->
          Printf.sprintf "%s: %s"
            "test, post op"
            (dbg_to_yojson (store_to_dbg s') |> Yojson.Safe.pretty_to_string));
      step (count-1,s')
    in
    if count <= 0 then () else ops |> List.iter f
  in
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  step (depth,!Test_store.test_store);
  Printf.printf "%s: ...tests finished\n%!" __FILE__;
  Printf.printf "%s: %d tests executed in total\n%!" __FILE__ !num_tests
[@@ocaml.warning "-8"]






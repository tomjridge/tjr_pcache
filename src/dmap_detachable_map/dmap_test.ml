(** Test Dmap on Pcl on Pl on a simple block device *)
open Tjr_store
open Store_passing

let ptr0 = 0

let make_dmap_ops ~store = 
  let (free_ref,pl_ref,blks_ref),s,pl_ops = 
    Pl_test.make_pl_test 
      ~store 
      ~data0:[] 
      ~ptr0 
      ~next_free_ptr:(fun x -> x+1) 
  in
  let s,pcl_ops = Pcl_test.make_pcl_test ~pl_ops ~store:s in
  (* make_dmap_ops needs a with_dmap *)
  let dcl_state0 = Dcl_types.{
    start_block=ptr0;
    current_block=ptr0;
    block_list_length=1;
    abs_past=Tjr_polymap.empty Pervasives.compare;
    abs_current=Tjr_polymap.empty Pervasives.compare;
  }
  in
  let s,dmap_ref = mk_ref dcl_state0 s in
  let with_dmap f = Store_passing.with_ref dmap_ref f in
  let dmap_ops = 
    Detachable_map.make_dmap_ops
      ~monad_ops
      ~pcl_ops
      ~with_dmap:{with_state=with_dmap}
  in
  (free_ref,pl_ref,blks_ref,dmap_ref),s,dmap_ops


(* for a given state, we need to calculate:

- pl: butlast and last of e list list, map_past and map_current, and their associated bindings
- dmap: map_past and map_current, and their bindings

We work with bindings so that we can easily compare for equality

*)

type ('k,'v) pl_and_dmap_bindings = {
  pl_past:('k*'v)list;
  pl_current:('k*'v)list;
  dmap_past:('k*'v)list;
  dmap_current:('k*'v)list;
}
  
open Dcl_types

let calculate_bindings ~blks_ref (* ~pl_ref *) ~dmap_ref ~store =
  let blks = Tjr_store.get blks_ref store in
  let read_node = Pl_simple_implementation.Write_node.read_node in
  let ops_list = Persistent_list.pl_to_list ~read_node ~ptr:ptr0 ~blks in
  let butlast,last = ops_list |> List.rev |> function
      | x::xs -> List.rev xs,x
      | _ -> failwith __LOC__
  in
  let pl_past' = List.concat butlast in  (* NOTE not bindings *)
  let pl_past' = Op_aux.op_list_to_map pl_past' in
  let pl_past = Tjr_polymap.bindings pl_past' in
  let pl_current' = Op_aux.op_list_to_map last in
  let pl_current = Tjr_polymap.bindings pl_current' in
  let dmap_state = Tjr_store.get dmap_ref store in
  let dmap_past = dmap_state.abs_past |> Tjr_polymap.bindings in
  let dmap_current = dmap_state.abs_current |> Tjr_polymap.bindings in
  { pl_past; pl_current; dmap_past; dmap_current }

let _ = calculate_bindings

let run = Tjr_monad.State_passing.run

let exhaustive_check ~depth =
  let num_tests = ref 0 in
  let ks = [1;2;3] in
  (* FIXME could do better with ops *)
  let ops = `Detach::
            (ks 
             |> List.map (fun k -> [`Find(k);`Insert(k,2*k);`Delete(k)])
             |> List.concat)
  in
  let store = Tjr_store.initial_store in
  let (free_ref,pl_ref,blks_ref,dmap_ref),s,dmap_ops = make_dmap_ops ~store in
  let map_ops = 
    Detachable_map.convert_dmap_ops_to_map_ops ~monad_ops ~dmap_ops 
  in
  let calculate_bindings = calculate_bindings ~blks_ref ~dmap_ref in
  (* NOTE d is depth in following *)
  let rec go1 ~d ~s = go2 ~d ~s ~bindings:(calculate_bindings ~store:s)
  and go2 ~d ~s ~bindings = 
    (* for each op, calculate a next state *)
    ops 
    |> List.map (fun op -> 
        match op with
        | `Detach -> 
          run ~init_state:s (map_ops.detach ()) |> fun (_,s') -> (op,s')
        | `Find k -> 
          run ~init_state:s (map_ops.find k) |> fun (_,s') -> (op,s')
        | `Insert(k,v) -> 
          run ~init_state:s (map_ops.insert k v) |> fun (_,s') -> (op,s')
        | `Delete k -> 
          run ~init_state:s (map_ops.delete k) |> fun (_,s') -> (op,s'))
    |> List.iter (fun (op,s') -> 
        go3 ~d ~s ~bindings ~op ~s' ~bindings':(calculate_bindings ~store:s'))
  and go3 ~d ~s ~bindings ~op ~s' ~bindings' =
    (* check and recurse *)
    num_tests:=!num_tests+1;
    assert(true);
    go2 ~d:(d-1) ~s:s' ~bindings:bindings'
  in
  go1 ~d:depth ~s



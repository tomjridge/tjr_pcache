(** Test PCL, building on tests for PL *)

(* test  ---------------------------------------------------------- *)

(** Tests for pcl *)

open Pcl_types
open Tjr_store
open Fstore_passing

open Simple_pl_and_pcl_implementations

(** Number of elements to insert *)
let count = 20

(** Number of elements that can be stored in a node *)
let elts_per_node = 3


module Make(S:sig
    type ptr = int
    val ptr0: ptr
    val initial_store: Tjr_store.fstore
end) = struct
  open S

  include Pl_test.Make(struct
      type data = int list
      type ptr = int
      let ptr0 = ptr0
      let initial_store = initial_store
      let data0 = []
    end)

  let with_pcl,store = 
    mk_ref Pcl_impl.{es=[]} store |> fun (pcl_state_ref,s) -> 
    let with_pcl_state = Fstore_passing.fstore_ref_to_with_state pcl_state_ref in
    with_pcl_state,s
  
  let pcl_state_ops = 
    Pcl_impl.make_pcl_state_ops ~too_large:(fun es -> List.length es > elts_per_node) 

  let pcl_ops = 
    Persistent_chunked_list.make_pcl_ops 
      ~monad_ops ~pl_ops ~pcl_state_ops ~with_pcl  
end


let main () = 
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let ptr0 = 0 in
  let module M = Make(
    struct
      type ptr = int
      let ptr0 = 0
      let initial_store = Tjr_store.empty_fstore ~allow_reset:true ()
    end)
  in
  let ops = M.pcl_ops in
  let s = ref M.store in
  let run m = State_passing.to_fun m !s |> fun (r,s') -> s:=s'; r in
  1 |> iter_break (fun i -> 
      match i <= count with
      | true -> ignore(run (ops.insert i)); Cont (i+1)
      | false -> Break ());
  run (ops.pcl_write ()); (* NOTE need the last sync *)
  let blks = Tjr_store.get M.blks_ref !s in
  let ess = Persistent_chunked_list.pcl_to_elt_list_list ~read_node:M.read_node ~ptr:ptr0 ~blks in
  let expected = 
    [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]; [10; 11; 12]; [13; 14; 15]; [16; 17; 18]; [19; 20]]
  in
  Alcotest.(check (list (list int))) "" ess expected;
  Printf.printf "%s: ...tests finished\n" __MODULE__;
  ()

let test_set = [
  "Pcl_test",`Quick,main
]


(* to test interactively:

   FIXME remove this thread dependency

   #thread;;  
   #require "imp_fs";;

   open Imp_fs;;
   open Persistent_log;;
   Test.main();;


   - : (int, int) Imp_fs.Persistent_log.Test.state =
   {Imp_fs.Persistent_log.Test.map =
   [(2, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = Some 2; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (1, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = Some 1; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>});
   (0, {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>})];
   free = 3;
   plist_state =
   {Imp_fs.Persistent_log.Pl.current_ptr = 2;
   current_node = {Imp_fs.Persistent_log.Pl.next = None; contents = <abstr>}};
   pclist_state =
   {Imp_fs.Persistent_log.Pcl.elts = [Insert (20, 40)]; elts_repr = <abstr>}}

   This looks OK. The current elts is a singleton [(20,40)] and we
   have just started writing to node 2.

*)


(* FIXME do more testing a la gom testing with abstract models etc *)



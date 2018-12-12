(** Test PCL, building on tests for PL *)

(* test  ---------------------------------------------------------- *)

(* open Tjr_pcache *)
open Pcl_types

open Tjr_store
open Store_passing

open Pcl_simple_implementation


let make_pcl_test ~pl_ops ~store =
  mk_ref {es=[]} store |> fun (s,pcl_state_ref) -> 
  let with_pcl_state f = with_ref pcl_state_ref f in
  

  let pcl_state_ops = 
    make_pcl_state_ops ~too_large:(fun es -> List.length es >= 3) in
  let pcl_ops = 
    Persistent_chunked_list.make_pcl_ops 
      ~monad_ops ~pl_ops ~pcl_state_ops ~with_pcl:{with_state=with_pcl_state}
  in
  s,pcl_ops

let _ = make_pcl_test


(* main ----------------------------------------------------------- *)


(* run some tests *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let ptr0 = 0 in
  let _blks_ref,s,pl_ops = 
    Pl_test.make_pl_test
      ~store:Tjr_store.initial_store
      ~data0:[]
      ~ptr0
      ~next_free_ptr:(fun x -> x+1)
  in
  let s,pcl_ops = make_pcl_test ~pl_ops ~store:s in
  let s = ref s in
  List.iter 
    (fun i -> 
       Tjr_monad.State_passing.run ~init_state:(!s) (pcl_ops.insert i) 
       |> fun (_,s') -> s:=s')
    (Tjr_list.from_to 1 100);
  Printf.printf "%s: ...tests finished\n" __MODULE__;
  s  (* FIXME? *)



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




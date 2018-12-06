(** Test PCL, building on tests for PL *)

(* test  ---------------------------------------------------------- *)

open Tjr_pcache
open Pcl_types
open Ins_del_op_type
open Test_store

(* on-disk representation ----------------------------------------- *)

(* we have to fix on a representation for the list of ops; for the
   time being, let's just keep this abstract; eventually it will be
   bytes *)

module Repr : sig 
  type ('k,'v) repr
  val make_repr_ops : int -> ( ('k,'v)op, ('k,'v)repr) repr_ops
end = struct
  type ('k,'v) repr = ('k,'v) op  list
  let make_repr_ops n = 
    {
      nil=[];
      snoc=(fun e es -> 
          if List.length es + 1 <= n 
          then `Ok(es@[e]) 
          else `Error_too_large);
      repr_to_list=(fun r -> r)
    }
end
open Repr



module Make(S:sig 
    type ptr=int 
    type k 
    type v 
    val repr_ops: ((k,v)op, (k,v)repr) repr_ops
end) = struct
  open S

  (** PCL state. Each node contains a list of kv op, represented by kv repr. *)
  type (* ('k,'v) *) pcl_state' = 
    ( (k,v)op,  (* elements *)
      (k,v)repr) (* representation *)
      pcl_state


  (* instantiate pl_test ---------------------------------------------- *)

  (** NOTE the initial contents is just the representation of the empty list of ops *)
  module A' = struct 
    type ptr = S.ptr
    type node_contents=(k,v)repr  
    let init_contents = repr_ops.nil  (* must agree with thet pcl_state below? *)
  end

  module Pl_test' = Pl_test.Make(A')
  open Pl_test'

  (* init state *)
  let pcl_ref = 
    let elts = [] in
    let elts_repr = repr_ops.nil in
    let pcl_state= { elts; elts_repr } in
    mk_ref' pcl_state


  (* with_pcl ------------------------------------------------------- *)

  let with_pcl f = with_ref pcl_ref f

  let _ = with_pcl


  (* pcl ops ----------------------------------------------- *)

  let pcl_ops =
    Persistent_chunked_list.make_pcl_ops
      ~monad_ops
      ~pl_ops
      ~repr_ops
      ~with_pcl:{with_state=with_pcl}

  let _ : ('e,'ptr,'t) pcl_ops
    = pcl_ops

  let _ : 
    ((k, v) op, ptr, state Tjr_monad.State_passing.state_passing) pcl_ops
      = pcl_ops


end


(* main ----------------------------------------------------------- *)

let repr_ops = make_repr_ops 2

module B' = struct
    type ptr = int 
    type k=int 
    type v=int 
    let repr_ops = repr_ops
end
module Pcl_test' = Make(B')
open Pcl_test'.Pl_test'
open Pcl_test'

(* run some tests *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __MODULE__;
  let cmds = 
    Tjr_list.from_to 0 20 |> List.map (fun x -> (x,2*x)) |> fun xs ->
    let rec f xs = 
      match xs with 
      | [] -> return ()
      | (k,v)::xs -> 
        pcl_ops.insert (Insert(k,v)) >>= fun _ ->
        f xs
    in
    f xs
  in  
  Tjr_monad.State_passing.run ~init_state:(!Test_store.test_store) cmds 
  |> fun (x,s) -> 
  assert(x=());
  Printf.printf "%s: ...tests finished\n" __MODULE__;
  s


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




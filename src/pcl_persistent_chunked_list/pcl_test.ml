(* test  ---------------------------------------------------------- *)

(* now we want to use the Persistent_chunked_list, which rests on
   persistent_list *)
open Pl_types
open Pcl_types
open Ins_del_op_type


(* on-disk representation ----------------------------------------- *)
(* we have to fix on a representation for the list of ops; for the
   time being, let's just keep this abstract; eventually it will be
   bytes *)

module Repr : sig 
  type ('k,'v) repr
  val repr_ops : int -> ( ('k,'v)op, ('k,'v)repr) repr_ops
end = struct
  type ('k,'v) repr = ('k,'v) op  list
  let repr_ops n = 
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




(* In order to test, we need a state which contains the plist state
   and the pclist state. *)

type ptr = int
type ('k,'v) list_node = (ptr,('k,'v)repr) Pl_types.list_node


(* system state *)
type ('k,'v) state = {
  map: (ptr * ('k,'v)list_node) list;  (* association list *)
  free: int;  (* iso to ptr *)

  (* plog_state: ('k,'v) plog_state *)
  plist_state: (int,('k,'v)repr) plist_state;
  pclist_state: (('k,'v)op,('k,'v)repr) pcl_state
}

let init_state ~repr_ops = 
  let start_block = Pl_test.start_block in
  (* NOTE we can't reuse Pl.Test.init_state because the values on
     disk are of a different type *)
  (* let i = Persistent_list.Test.init_state in *)
  let elts = [] in
  let elts_repr = repr_ops.nil in
  let current_node= { next=None; contents=elts_repr } in
  {    
    map=[(start_block,current_node)]; 
    free=(start_block+1);
    plist_state={
      current_ptr=start_block;
      current_node
    }; 
    pclist_state={ elts; elts_repr };
  }


(* monad ops ------------------------------------------------------ *)

open Tjr_monad.Types
open Tjr_monad.State_passing

let monad_ops : ('k,'v) state state_passing monad_ops = 
  Tjr_monad.State_passing.monad_ops ()

let ( >>= ) = monad_ops.bind 
let return = monad_ops.return

let with_world = Tjr_monad.State_passing.with_world



(* list ops ------------------------------------------------------- *)

let list_ops () : (('k, 'v) repr, 'ptr, ('k, 'v) state state_passing) Pl_types.list_ops = 
  Persistent_list.make_persistent_list
    ~monad_ops
    ~write_node:(fun ptr node -> 
        with_world (fun s -> ((),{ s with map=(ptr,node)::s.map })))
    ~plist_state_ref:{
      get=(fun () -> with_world (fun s -> (s.plist_state,s)));
      set=(fun plist_state -> with_world (fun s -> ((),{s with plist_state})))
    }
    ~alloc:(fun () -> with_world (fun s -> (s.free,{ s with free=s.free+1 })))

let _ = list_ops


(* chunked list ops ----------------------------------------------- *)


let chunked_list ~repr_ops =
  Persistent_chunked_list.make_persistent_chunked_list
    ~monad_ops
    ~list_ops:(list_ops ())
    ~repr_ops
    ~pcl_state_ref:{
      get=(fun () -> with_world (fun s -> (s.pclist_state,s)));
      set=(fun pclist_state -> with_world (fun s -> ((),{s with pclist_state})));
    }

let _ : repr_ops:('e,'repr)repr_ops -> ('e,'ptr,'t) pcl_ops
  = chunked_list



(* main ----------------------------------------------------------- *)

(* run some tests *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  let repr_ops = repr_ops 2 in
  let init_state = init_state ~repr_ops in
  chunked_list ~repr_ops |> function { insert } ->
    let cmds = 
      Tjr_list.from_to 0 20 |> List.map (fun x -> (x,2*x)) |> fun xs ->
      let rec f xs = 
        match xs with 
        | [] -> return ()
        | (k,v)::xs -> 
          insert (Insert(k,v)) >>= fun _ ->
          f xs
      in
      f xs
    in  
    Tjr_monad.State_passing.run ~init_state cmds |> fun (x,s) -> 
    assert(x=());
    Printf.printf "%s: ...tests finished\n" __FILE__;
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




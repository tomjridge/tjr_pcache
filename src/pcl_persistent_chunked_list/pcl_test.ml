(*
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



module Make(S:sig type ptr=int type k type v val init_contents: (k,v)repr end) = struct
  open S

  (** A node contains a repr (of a list of kv op *)
  type ('k,'v) pl_node' = (ptr,('k,'v)repr) pl_node


  (** PCL state. Each node contains a list of kv op, represented by kv repr. *)
  type ('k,'v) pcl_state' = 
    ( ('k,'v)op,  (* elements *)
      ('k,'v)repr) (* representation *)
      pcl_state

  (* instantiate pl_test ---------------------------------------------- *)

  module A_arg = struct 
    type ptr = S.ptr
    type node_contents=(k,v)repr  
    (* NOTE node_contents has no tyvar args, so we can't make
       node_contents = ('k,'v) repr *)
    type pcl_state=(k,v)pcl_state'
    let init_contents = init_contents
  end

  module A = Pl_test.Make(A_arg)
  open A

  (** The test state is as {!Pl_test}, but the PCL component is of type
      [pcl_state'] *)
  type (* ('ptr,'k,'v) *) state = 
    (* ('ptr, ('k,'v)op, ('k,'v) pcl_state') *) A.state


  let init_state ~repr_ops = 
    let elts = [] in
    let elts_repr = repr_ops.nil in
    let pcl_state= { elts; elts_repr } in
    init_state ~pcl_state


  (* monad ops ------------------------------------------------------ *)

  open Tjr_monad.Types
  open Tjr_monad.State_passing.State_passing_type

  let monad_ops : (* ('ptr,'k,'v) *) state state_passing monad_ops = 
    Tjr_monad.State_passing.monad_ops'

  let ( >>= ) = monad_ops.bind 
  let return = monad_ops.return

  let with_pcl f = 
    (* let open Pl_test in *)
    Tjr_monad.State_passing.with_state
      ~get:(fun x -> x.pcl_state) 
      ~set:(fun s t -> {t with pcl_state=s})
      ~f

  let _ = with_pcl


  (* pl ops ------------------------------------------------------- *)

  (* NOTE pl_ops are from A *)


  (* chunked list ops ----------------------------------------------- *)

  let chunked_list ~repr_ops =
    Persistent_chunked_list.make_persistent_chunked_list
      ~monad_ops
      ~pl_ops
      ~repr_ops
      ~with_pcl:{with_state=with_pcl}

  let _ : repr_ops:('e,'repr)repr_ops -> ('e,'ptr,'t) pcl_ops
    = chunked_list

  let _ : repr_ops:((k, v) op, A_arg.node_contents) repr_ops ->
    ((k, v) op, ptr, state Tjr_monad.State_passing.state_passing) pcl_ops
      = chunked_list


end

(* main ----------------------------------------------------------- *)

module B = Make(struct type ptr = int type k=int type v=int end)
open B

(* run some tests *)
let main () = 
  Printf.printf "%s: tests starting...\n%!" __FILE__;
  let repr_ops = repr_ops 2 in
  let init_state = init_state ~repr_ops ~init_contents:repr_ops.nil in
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



*)

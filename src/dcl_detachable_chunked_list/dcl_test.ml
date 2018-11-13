open Pcl_types
open Dcl_types

(* test  ---------------------------------------------------------- *)


open Dcl_dbg

include struct
  type ptr = int
  open Pl_types
  open Pcl_test.Repr

  type ('k,'v) list_node = (ptr,('k,'v)repr) Pl_types.list_node

  type ('k,'v,'map,'dbg) state = {
    map: (ptr * ('k,'v)list_node) list;  (* association list *)
    free: ptr;  (* iso to ptr *)

    plist_state: (int,('k,'v)repr) plist_state;
    pclist_state: (('k,'v)op,('k,'v)repr) pcl_state;
    plog_state: ('map,ptr) plog_state;
    dbg: 'dbg; (* debug state *)
  }
end


open Tjr_monad.Types
open Tjr_monad.State_passing

let monad_ops : ('k,'v,'map,'dbg) state state_passing monad_ops = 
  Tjr_monad.State_passing.monad_ops ()


(* NOTE FIXME copied from pcl *)
let list_ops () = Persistent_list.make_persistent_list
    ~monad_ops
    ~write_node:(fun ptr node -> with_world (fun s -> ((),{ s with map=(ptr,node)::s.map })))
    ~plist_state_ref:{
      get=(fun () -> with_world (fun s -> (s.plist_state,s)));
      set=(fun plist_state -> with_world (fun s -> ((),{s with plist_state})))
    }
    ~alloc:(fun () -> with_world (fun s -> (s.free,{ s with free=s.free+1 })))

let _ = list_ops

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

let plog ~map_ops = 
  chunked_list () |> fun { insert } -> 
  Detachable_chunked_list.make_plog_ops
    ~monad_ops
    ~map_ops
    ~insert
    ~plog_state_ref:{
      get=(fun () -> with_world (fun s -> (s.plog_state,s)));
      set=(fun plog_state -> with_world (fun s -> ((),{s with plog_state})))
    }

let _ : 
  map_ops:('k, ('k, 'v) op, 'map) Tjr_map.map_ops -> 
  ('k, 'v, 'map, ptr, ('k, 'v, 'map,'dbg) state state_passing) plog_ops 
  = plog


(* fix types of 'k 'v and 'map *)
(* test with an int -> int map *)

open Tjr_map

module Map_ = Tjr_map.Make(Int_ord)

let map_ops = Map_.map_ops

let plog () = plog ~map_ops


let _ : unit ->
  (ptr, 'a, (ptr, 'a) op Map_int.t, ptr,
   (ptr, 'a, (ptr, 'a) op Map_int.t,'dbg) state state_passing)
    plog_ops
  = plog


let checked_plog () : ('k,'v,'map,'ptr,'t) plog_ops = 
  let read_node ptr s = List.assoc ptr s.map in
  let plist_to_nodes ~(ptr:ptr) (s:('k,'v,'map,'dbg)state) = 
    Persistent_list.plist_to_nodes ~read_node ~ptr s 
  in
  let repr_to_list = repr_ops.repr_to_list in
  let pclist_to_nodes ~ptr s = 
    Persistent_chunked_list.pclist_to_nodes ~repr_to_list ~plist_to_nodes ~ptr s
  in
  let _ = pclist_to_nodes in
  let plog_to_dbg s = 
    plog_to_dbg
      ~pclist_to_nodes
      ~get_plog_state:(fun s -> s.plog_state)
      s
  in
  let plog_ops = plog () in
  let set_dbg = fun dbg s -> {s with dbg} in
  let get_dbg = fun s -> s.dbg in
  make_checked_plog_ops
    ~monad_ops
    ~plog_ops
    ~plog_to_dbg
    ~set_dbg
    ~get_dbg



let _ : (int,'v,'map,ptr,(int,'v,'map,'dbg)state state_passing)plog_ops = 
  checked_plog ()


(* testing ------------------------------------------------------ *)



let init_state = 
  let start_block = Pl_test.start_block in
  let i = Pcl_test.init_state ~repr_ops in
  {
    map=i.map;
    free=i.free;
    plist_state=i.plist_state;
    pclist_state=i.pclist_state;
    plog_state={
      start_block;
      current_block=start_block;
      block_list_length=1;
      map_past=map_ops.map_empty;
      map_current=map_ops.map_empty
    };
    dbg=init_dbg
  }
[@@ocaml.warning "-40"]


let test ~depth = 
  let num_tests = ref 0 in
  let plog_ops = checked_plog () in
  (* let plog_ops = plog () in *)
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
        run ~init_state:s (plog_ops.detach ()) |> fun (_,s') -> s'
      | `Delete k -> 
        Printf.sprintf "delete(%d)" k |> Pcache_debug.log;
        run ~init_state:s (plog_ops.add (Delete(k))) |> fun (_,s') -> s'
      | `Find k ->
        Printf.sprintf "find(%d)" k |> Pcache_debug.log;
        run ~init_state:s (plog_ops.find k) |> fun (_,s') -> s'
      | `Insert(k,v) -> 
        Printf.sprintf "insert(%d,%d)" k v |> Pcache_debug.log;
        run ~init_state:s (plog_ops.add (Insert(k,v))) |> fun (_,s') -> s'
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




(* FIXME what is this module for? *)
module Abstract_model_ops(Ptr:Tjr_int.TYPE_ISOMORPHIC_TO_INT) = struct

  (* FIXME we may want the state to be a map rather than an assoc
     list; but then we need polymorphic maps over k *)

  (* for pointers, we just make sure to allocate a new pointer every time we detach a non-empty prefix *)
  type ('k,'v) state = {
    kvs: ('k*('k,'v) op) list;  (* association list *)
    ptr_ref: Ptr.t
  }


  (* from List.ml *)
  let rec assoc_opt x = function
      [] -> None
    | (a,b)::l -> if compare a x = 0 then Some b else assoc_opt x l


  (* FIXME work with just a list; move this module out of here; then make a version which works with the model state *)
  module Kvop_list_as_kv_map = struct

    let map_empty = { kvs=[]; ptr_ref=Ptr.int2t 0 }
    let map_is_empty x = x.kvs = []

    let find k s = assoc_opt k s.kvs |> function
      | None -> None
      | Some(Delete k) -> None
      | Some(Insert(k,v)) -> Some v

    let delete k s =
      List.filter (fun (k',_) -> k' <> k) s.kvs |> fun kvs -> 
      { s with kvs }

    let insert k v s = 
      delete k s |> fun s ->
      (k,Insert(k,v))::s.kvs |> fun kvs ->
      { s with kvs }

    let map_add = insert
    let map_remove = delete
    let map_find = find
    let map_bindings = 
      fun s -> 
        s.kvs |> 
        List.map snd |> 
        List.filter (function Delete _ -> false | _ -> true) |> 
        List.map (function Insert(k,v) -> (k,v) | _ -> failwith "impossible")

    let _kvop_map_ops = Tjr_map.{ map_empty; map_is_empty; map_add; map_remove; map_find; map_bindings }

  end

  open Tjr_monad.Mref

  (** 
     Parameters:
     - [mref] - the mref for the abstract state as a subpart of the global state
  *)
  let _abstract_model_ops ~monad_ops ~ops_per_block ~mref = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in      
    let find k =
      mref.get () >>= fun s -> assoc_opt k s.kvs |> return
    in
    let add op =
      mref.get () >>= fun s -> 
      let k = op2k op in
      (* NOTE we make sure there are no duplicate keys in the list FIXME add as assoc_insert in tjr_lib *)
      (k,op)::(List.filter (fun (k',_) -> k' <> k) s.kvs) |> fun kvs ->
      {s with kvs } |> mref.set
    in
    let detach () =
      mref.get () >>= fun s ->
      let n = List.length s.kvs in
      let n_remaining = n mod ops_per_block in
      let remaining = Tjr_list.take n_remaining s.kvs in
      let dropped = Tjr_list.drop n_remaining s.kvs in
      match n = n_remaining with
      | true -> return (s.ptr_ref,[],s.ptr_ref,s.kvs)
      | false -> 
        let ptr' = s.ptr_ref |> Ptr.t2int |> fun x -> x+1 |> Ptr.int2t in
        mref.set { kvs=remaining; ptr_ref=ptr' } >>= fun () ->
        return (s.ptr_ref,dropped,ptr',remaining)
    in
    let get_block_list_length () = 
      mref.get () >>= fun s ->
      return (1+ (List.length s.kvs / ops_per_block) )
    in
    {find; add; detach; get_block_list_length}


  let _init_state = { kvs=[]; ptr_ref=Ptr.int2t 0 }



end

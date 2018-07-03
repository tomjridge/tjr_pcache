(** The Global Object Map (GOM). *)

(** At a high level, this is an int -> int map, from object id to
   block id (the root of the object on disk).

The in-memory map is maintained as in an LRU in memory. Thus, this
   implementation only has to deal with persistent aspects.

When the LRU becomes full, or on sync of a particular object id, the
   GOM receives a list of map operations.

These are written straight to the pcache.


At intervals, when the pcache becomes long, some initial prefix of the
   pcache is rolled into the B-tree. The root of the pcache is
   adjusted (the old blocks can be reclaimed), and the root is written
   to disk (but not necessarily synced). On crash, if the old root is
   used there is no problem - we just replay these modifications over
   the B-tree. We require that if the new pcache root hits disk, the
   B-tree is also on disk. One approach is to async (flush btree;
   flush pcache root).

*)

open Tjr_monad.Monad


(*
module type GOM_REQUIRES = sig

  (* although we deal with fixed k=obj_id and v=blk_id, we might as
     well treat these as parameters, since all the other libraries are
     parametric anyway and nothing depends on the exact nature of
     these *)

  (** The B-tree operations *)
  (* FIXME just use existing Tjr_btree.Map_ops.map_ops 
  type ('k,'v,'t) map_ops = {
    find: 'k -> ('v option,'t) m;
    insert: 'k -> 'v -> (unit,'t) m;
    delete: 'k -> (unit,'t) m;
    insert_many: 'k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m
  }
  *)
  (* NOTE we may execute find on the B-tree if we execute GOM.find and nothing in pcache or LRU *)
  type ('k,'v,'t) map_ops = ('k,'v,'t) Tjr_btree.Map_ops.map_ops


  (** The pcache operations *)
  type ('k,'v,'map,'ptr,'t) plog_ops = ('k,'v,'map,'ptr,'t) Persistent_log.plog_ops

  (* What API do we expose? We need to support what comes from the
     LRU, which is roughly the map operations, but likely batched. In
     fact, at the moment the LRU does not produce insert_many
     operations, but probably it should. So really we need to support
     all the map operations. *)

  (* NOTE we provide map_ops *)

  type blk_id
end
*)


module Make_gom(Gom_requires : sig type blk_id end) = struct
  
  open Gom_requires

  open Tjr_btree.Map_ops

  open Persistent_log

  (** The gom state consists of a flag indicating that we are executing a roll-up *)
  type gom_state = {
    in_roll_up: bool;
    pcache_root: blk_id;
    btree_root: blk_id;
  }

  open Tjr_monad.Mref_plus

  type 't gom_state_ops = (gom_state,'t) mref

  (* we perform a "roll up" operation, merging the pcache into the
     B-tree, when the number of pcache blocks reaches
     pcache_blocks_limit *)

  let make_gom_ops
      ~monad_ops ~btree_ops ~pcache_ops ~pcache_blocks_limit 
      ~gom_mref_ops ~(detach_map_ops:('k,'v,'map) Persistent_log.detach_map_ops) 
      ~bt_sync  (* to sync the btree *)
      ~sync_pcache_roots  (* to write the pcache roots to disk somewhere *)
    =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    dest_map_ops btree_ops @@ fun ~find ~insert ~delete ~insert_many ->
    (* rename just so we don't get confused *)
    let (bt_find,bt_insert,bt_delete,bt_insert_many) = (find,insert,delete,insert_many) in
    let (*Persistent_log.*){find; add; detach; get_block_list_length} = pcache_ops in
    let (pc_find,pc_add,pc_detach,pc_get_block_list_length) = (find,add,detach,get_block_list_length) in
    let find k = 
      pc_find k >>= fun op ->
      match op with
      | None -> bt_find k
      | Some op ->
        match op with
        | Insert(k,v) -> return (Some v)
        | Delete k -> return None
    in
    let maybe_roll_up () = 
      gom_mref_ops.get () >>= function { in_roll_up } ->
        match in_roll_up with
        | true -> 
          return `Already_in_roll_up
        | false ->
          pc_get_block_list_length () >>= fun n ->
          match n >= pcache_blocks_limit with
          | false -> return `No_roll_up_needed
          | true -> 
            (* we need to roll-up the pcache into the B-tree *)
            (* first set the flag; if already set, just skip the roll
               up since someone else is doing it *)
            gom_mref_ops.with_ref (fun s -> 
                match s.in_roll_up with
                | true -> `Already_in_roll_up,s
                | false -> `Ok,{s with in_roll_up=true}) >>= 
            begin
              function
              | `Already_in_roll_up ->
                (* of course, we checked in_roll_up above, and it was
                   false; but a concurrent thread may have set it in the
                   meantime *)
                return `Already_in_roll_up
              | `Ok -> 
                (* the flag has been set; we need to roll up the cache;
                   FIXME we also need some sort of
                   backpressure/prioritization on this roll-up thread in
                   case the cache is running too far ahead of the B-tree *)
                pc_detach () >>= fun (old_root,(map:'map),new_root) ->
                (* map consists of all the entries we need to roll up *)
                map |> detach_map_ops.map_bindings |> fun kvs ->
                let rec loop kvs = 
                  match kvs with
                  | [] -> return  (`Finished(old_root,new_root))
                  | (k,v)::kvs ->
                    match v with
                    | Insert (k,v) -> bt_insert k v >>= fun () -> loop kvs
                    | Delete k -> bt_delete k >>= fun () -> loop kvs
                in
                loop kvs
            end
            >>= 
            begin
              function
              | `Already_in_roll_up -> return `Already_in_roll_up
              | `Finished(old_root,(*new*)pcache_root) -> 
                (* sync the btree *)
                (* NOTE this should be done in the critical section, before resetting the flag *)
                bt_sync () >>= fun btree_root ->
                (* now we need to reset the flag, and the roots *)
                sync_pcache_roots ~btree_root ~pcache_root >>= fun () ->
                gom_mref_ops.with_ref (fun s -> 
                    assert(s.in_roll_up);
                    (),{ in_roll_up=false; btree_root; pcache_root }) >>= fun () ->
                return `Ok
            end              
    in
    let insert k v =
      pc_add (Insert(k,v)) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let delete k =
      pc_add (Delete k)
    in
    let insert_many k v kvs = 
      (* FIXME we should do something smarter here *)
      insert k v >>= fun () -> return kvs
    in
    { find; insert; delete; insert_many }
    

  (* FIXME when we detach, we should not alter the pcache root, but
     later after the btree changes are synced, we can sync the new
     pcache root; or perhaps we store both roots somewhere else - in
     the gom controller *)

end

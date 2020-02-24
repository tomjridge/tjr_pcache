(** A simplified "Make" functor, to construct a persistent cache. *)

open Pcache_intf
open Pcache_intf.Pvt

module type S = sig
  include MRSHL
  val k_cmp: k -> k -> int
end

module Make_1(S:S) = struct  
  open S

  module Map = struct
    include Map.Make(struct type t = k let compare = k_cmp end)
    let merge ~older ~(newer:'a t) = 
      (older,to_seq newer) |> iter_k (fun ~k:kont (older,newer) ->
          match newer () with
          | Seq.Nil -> older
          | Seq.Cons((k,op),rest) -> 
            kont (add k op older,rest))
  end
  open Map

  let kvop_map_ops : (k,kvop,kvop Map.t)pcache_map_ops = {
    empty;
    find_opt;
    insert=add;
    delete=remove;
    merge
  }

  (* what we use with Tjr_pcache.Make_with_fixed_types *)
  module S2 = struct
    include S

    type t = lwt

    let monad_ops = lwt_monad_ops

    let marshalling_config : (k,v,r)marshalling_config = (module S)

    (* let kvop_map_ops = Kvop.default_kvop_map_ops ()     *)

    type kvop_map = kvop Map.t

    let kvop_map_ops = kvop_map_ops
        
    type nonrec dmap_state = (r,kvop_map)dmap_state

    type blk_id = r

    type blk = ba_buf

    let blk_ops = Blk_factory.make_3 ()

  end
  include S2

  (* the following seem to make merlin type inference nicer (but why
     doesn't short-paths use the short paths here when we include S2?)
     *)
      (*
  type t = S2.t
  type kvop_map = S2.kvop_map
  type nonrec dmap_state = dmap_state
*)
  
  module Pcache = Tjr_pcache.Make_with_fixed_types(S2)

  let make 
      ~(blk_alloc:(unit -> (r,t)m))
      ~(with_dmap:(dmap_state,t)with_state) 
      ~(write_to_disk:dmap_state -> (unit,t)m)
    : (k,v,r,kvop_map,t) dmap_ops = 
    Pcache.make {monad_ops; marshalling_config; kvop_map_ops; blk_ops; blk_alloc; with_dmap; write_to_disk }

  let _ = make

  type nonrec dmap_ops = (k,v,r,kvop_map,t) dmap_ops

  let dmap0 ~r = Pcache_intf.Pvt.dmap0 ~r ~empty:kvop_map_ops.empty

(*
  module Pvt : sig 
    type pcache_descr = private dmap_ops
  end = struct
    type pcache_descr = dmap_ops
  end
*)
(*
  type pcache_descr = { 
    (* blk_alloc:(unit -> (r,t)m);  *)
    (* with_dmap:(dmap_state,t)with_state; *)
    (* write_to_disk:dmap_state -> (unit,t)m; *)
    dmap_ops:dmap_ops
  }

  let find ~pd = pd.dmap_ops.find 
  let insert ~pd = pd.dmap_ops.insert
  let delete ~pd = pd.dmap_ops.delete
  let detach ~pd = pd.dmap_ops.detach ()
  let block_list_length ~pd = pd.dmap_ops.block_list_length
  let dmap_write ~pd = pd.dmap_ops.dmap_write
  let dmap_sync ~pd = pd.dmap_ops.dmap_sync ()
  let read_pcache ~pd = pd.dmap_ops.read_pcache
*)
end


open Pcache_ctxts

module type C_all = sig
  open Pcache_example_intf
  type k
  type v
  type op = (k,v)Fixed.op
  include C_rt
  type pl_data = Fixed.buf_pos (* we want to write a whole block of
                                  data, so buf should probably be
                                  blk_sz bytes *)
  (* NOTE pl_data is shared with pcl, dcl and dmap *)
  type pl_internal = { 
    current :r; 
    next    :r option; 
    pl_data :pl_data; 
    synced  :bool 
  }
  (* for pl_internal, perhaps we write the next pointer in the first
     10 bytes, so that we don't have to repeatedly do it when pl_data
     is updated *)

  module Pl_on_disk : sig
    type t = {
      next: r option;
      pl_data: pl_data
    }
  end

  include C_pl with type r:=r 
                and type t:=t 
                and type pl_data:=pl_data 
                and type pl_internal:=pl_internal
  type e = op
  type pcl_internal = { pcl_internal: pl_data } (* op list as buf+pos *)
  include C_pcl with type r:=r 
                 and type t:=t 
                 and type pl_data:=pl_data 
                 and type e:=e 
                 and type pcl_internal:=pcl_internal
  type kv_map
  type abs = kv_map
  val kv_map_ops: (k,v,kv_map) Tjr_map.map_ops
  include C_dcl with type r:=r 
                 and type t:=t 
                 and type op:=op 
                 and type abs:=abs
  include C_dmap with type r:=r 
                  and type t:=t 
                  and type k:=k 
                  and type v:=v
  val marshalling_config: (k,v,r) marshalling_config

  type dmap_node_on_disk = {
    next: r option;
    elts: e list
  }

  type blk
  type blk_id = r
  val blk_dev_ops: (blk_id,blk,t) blk_dev_ops
  val with_pl: (pl_internal,t)with_state
  val with_pcl: (pcl_internal,t)with_state
  val with_dmap: (dmap_state,t)with_state

  module Make():sig
    (* pl is the natural cache point; we assume the other layers
       automatically call down to lower layers when the data changes;
       then sync forces the changes to disk *)
    val sync_pl: unit -> (unit,t)m

    val read_node: r -> (dmap_node_on_disk,t)m
    val read_nodes: r -> (dmap_node_on_disk list,t)m (* last one is the end of the list *)
  end
end

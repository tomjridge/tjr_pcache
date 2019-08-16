(** Various other bits of configuration (typically how to marshal k,v etc) *)
module Blk_id = Blk_id_as_int

module Marshalling_config_type = struct
  
  type ('k,'v,'ptr) marshalling_config = {
    ptr_sz:int;
    blk_sz:int;
    k_size:int;
    v_size:int;
    k_writer: 'k Bin_prot.Type_class.writer;
    k_reader: 'k Bin_prot.Type_class.reader;
    v_writer: 'v Bin_prot.Type_class.writer;
    v_reader: 'v Bin_prot.Type_class.reader;
    ptr_writer: 'ptr Bin_prot.Type_class.writer;
    ptr_reader: 'ptr Bin_prot.Type_class.reader;
    (* ptr0:'ptr; (\* initial block *\) *)
    next_free_ptr:'ptr -> 'ptr;
  }

end
include Marshalling_config_type

(** {2 Type that captures all the different layers} *)

(** NOTE in the following type, the option refs have to be
    initialized before calling dmap_ops *)
type ('k,'v,'r,'t,'blk,'pl_data,'pl_internal_state,'pcl_internal_state,'pcl_elt,'fd,'e) pcache_layers = {
  monad_ops     : 't monad_ops;

  config        : ('k,'v,'r) marshalling_config;
  elt_writer    : 'pcl_elt Bin_prot.Type_class.writer;
  elt_reader    : 'pcl_elt Bin_prot.Type_class.reader;

  blk_write_count: int ref;
  blk_ops       : 'blk blk_ops;
  blk_dev_ops   : 'fd -> ('r,'blk,'t)blk_dev_ops;
  write_node    : blk_dev_ops:('r,'blk,'t)blk_dev_ops -> pl_state:'pl_internal_state -> (unit,'t)m;
  read_back     : blk_dev_ops:('r,'blk,'t)blk_dev_ops -> blk_id:'r -> ('e list list,'t)m;
  (* NOTE 'e is likely ins/del, different from pcl_elt.elt *)

  pl_state_ops  : ('pl_data,'r,'pl_internal_state)Pcache_intf.Pl_types.pl_state_ops;
  pcl_state_ops : ('pl_data,('k,'v)op,'pcl_internal_state)Pcl_types.pcl_state_ops;

  alloc         : (unit -> ('r,'t)m)option ref;
  with_pl       : ('pl_internal_state,'t) with_state option ref;
  with_pcl      : ('pcl_internal_state,'t) with_state option ref;
  with_dmap     : (('r,'k,'v)Dmap_types.dmap_state,'t) with_state option ref;

  (* why does dmap_ops need pl write_node? because dmap is isolated from blk_dev_ops *)
  dmap_ops      : write_node:('pl_internal_state -> (unit,'t)m) ->  ('k,'v,'r,'t)Dmap_types.dmap_ops
}



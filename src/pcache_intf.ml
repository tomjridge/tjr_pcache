module Kvop = Tjr_fs_shared.Kv_op

(** The result of "detaching" the map. We get the abstract map for
    all but the current node, and information about the current
    node. *)
type ('k,'v,'r,'kvop_map) detach_info = { 
  root_ptr    : 'r;
  past_map    : 'kvop_map;
  current_ptr : 'r;
  current_map : 'kvop_map;
}

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type buf_ops = {
  create: int -> buf;  (* assumed to be zero-ed *)
  get: int -> buf -> char
}

let buf_ops : buf_ops = Bigstring.{
    create;
    get=(fun i b -> get b i)
}

(** For the detach operation, we get the map upto the current node,
    and the map for the current node. NOTE if root_ptr = current_ptr, then nothing was detached. *)
type ('k,'v,'r,'kvop_map,'t) dmap_ops = {
  find              : 'k -> ('v option,'t) m;
  insert            : 'k -> 'v -> (unit,'t) m;
  delete            : 'k -> (unit,'t)m;
  detach            : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
  block_list_length : unit -> (int,'t)m;
  dmap_write        : unit -> (unit,'t)m;
  dmap_sync         : unit -> (unit,'t)m;
  read_pcache       : root:'r -> read_blk_as_buf:('r -> buf) -> (('k,'v)kvop list * 'r option) list
}

    

type ('r,'kvop_map) dmap_state = {
  root_ptr              : 'r;
  past_map              : 'kvop_map;
  current_ptr           : 'r;
  current_map           : 'kvop_map;
  buf                   : buf;  (* should be the same size as a blk *)
  buf_pos               : int;
  next_ptr              : 'r option; 
  block_list_length     : int;
  dirty                 : bool; (* only if buf is dirty ie data changed, or next_ptr *)
}


module type MC = sig
  type k [@@deriving bin_io]
  type v [@@deriving bin_io]  
  type r [@@deriving bin_io]
  type nonrec kvop = (k,v)kvop [@@deriving bin_io]

  (** This is the max # of bytes required for k *)
  val k_size: int 
  val v_size: int
  val r_size: int
end

type ('k,'v,'r) marshalling_config = (module MC with type k='k and type v='v and type r='r)


(*
type ('k,'v,'ptr) marshalling_config = {
  ptr_sz        : int;
  blk_sz        : int;
  k_size        : int;
  v_size        : int;
  k_writer      : 'k Bin_prot.Type_class.writer;
  k_reader      : 'k Bin_prot.Type_class.reader;
  v_writer      : 'v Bin_prot.Type_class.writer;
  v_reader      : 'v Bin_prot.Type_class.reader;
  ptr_writer    : 'ptr Bin_prot.Type_class.writer;
  ptr_reader    : 'ptr Bin_prot.Type_class.reader;
}
*)

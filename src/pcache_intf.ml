(* let chr0 = Char.chr 0 *)

(* module Kvop = Tjr_fs_shared.Kvop *)

(* FIXME why are these here? *)

type buf = ba_buf
(* (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t *)

type buf_ops = {
  create: int -> buf;  (* assumed to be zero-ed *)
  get: int -> buf -> char
}

let buf_ops : buf_ops = Bigstring.{
    create=(fun n -> Bigstring.make n chr0);
    get=(fun i b -> get b i)
}


(** The result of "detaching" the map. We get the abstract map for
    all but the current node, and information about the current
    node. *)
type ('k,'v,'r,'kvop_map) detach_info = { 
  root_ptr    : 'r;
  past_map    : 'kvop_map;
  current_ptr : 'r;
  current_map : 'kvop_map;
}


open Kvop

(** For the detach operation, we get the map upto the current node,
   and the map for the current node. NOTE if root_ptr = current_ptr,
   then nothing was detached. *)
type ('k,'v,'r,'kvop_map,'t) pcache_ops = {
  find              : 'k -> ('v option,'t) m;
  insert            : 'k -> 'v -> (unit,'t) m;
  delete            : 'k -> (unit,'t)m;
  detach            : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
  blk_len : unit -> (int,'t)m;
  pcache_write        : unit -> (unit,'t)m;
  pcache_sync         : unit -> (unit,'t)m;
  (* read_pcache       : root:'r -> read_blk_as_buf:('r -> (buf,'t)m) ->  *)
    (* ((('k,'v)kvop list * 'r option) list * 'buf,'t)m *)
}

module Pcache_state = struct
  type ('r,'kvop_map) pcache_state = {
    root_ptr          : 'r;
    past_map          : 'kvop_map;
    current_ptr       : 'r;
    current_map       : 'kvop_map;
    buf               : buf;  (* should be the same size as a blk *)
    buf_pos           : int;
    next_ptr          : 'r option; 
    blk_len : int;
    dirty             : bool; (* only if buf is dirty ie data changed, or next_ptr *)
  }

  let empty_pcache_state ~root_ptr ~current_ptr ~empty = {
    root_ptr;
    past_map=empty;
    current_ptr;
    current_map=empty;
    buf=ba_buf_ops.create (Blk_sz.to_int blk_sz_4096);
    buf_pos=0;
    next_ptr=None;
    blk_len=1;
    dirty=true
  }

(*
  let initial_pcache_state ~root_ptr ~current_ptr ~empty = {
    root_ptr;
    past_map=empty;
    current_ptr;
    current_map=empty;
    buf=ba_buf_ops.create (Blk_sz.to_int blk_sz_4096);
    buf_pos=0;
    next_ptr=None;
    blk_len=1;
    dirty=true
  }
*)

end

module type MRSHL = sig
  type k [@@deriving bin_io, yojson]
  type v [@@deriving bin_io, yojson]  
  type r [@@deriving bin_io, yojson]
  type nonrec kvop = (k,v)kvop [@@deriving bin_io, yojson]

  (** This is the max # of bytes required for k *)
  val k_size: int 
  val v_size: int
  val r_size: int
end

type ('k,'v,'r) marshalling_config = (module MRSHL with type k='k and type v='v and type r='r)


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

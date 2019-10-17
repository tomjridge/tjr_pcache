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

(** For the detach operation, we get the map upto the current node,
    and the map for the current node. NOTE if root_ptr = current_ptr, then nothing was detached. *)
type ('k,'v,'r,'kvop_map,'t) dmap_ops = {
  find              : 'k -> ('v option,'t) m;
  insert            : 'k -> 'v -> (unit,'t) m;
  delete            : 'k -> (unit,'t)m;
  detach            : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
  block_list_length : unit -> (int,'t)m;
  dmap_write        : unit -> (unit,'t)m;
  dmap_sync         : unit -> (unit,'t)m
}

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type buf_ops = {
  create: int -> buf;  (* assumed to be zero-ed *)
  get: int -> buf -> char
}

let buf_ops : buf_ops = failwith ""

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

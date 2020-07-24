(** Main pcache interfaces; don't open *)


(* let chr0 = Char.chr 0 *)

(* module Kvop = Tjr_fs_shared.Kvop *)

(* $(FIXME("why are these buf ops here?")) *)

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
(* $(PIPE2SH("""sed -n '/type[ ].*pcache_ops/,/^}/p' >GEN.pcache_ops.ml_""")) *)
type ('k,'v,'r,'kvop_map,'t) pcache_ops = {
  find         : 'k -> ('v option,'t) m;
  insert       : 'k -> 'v -> (unit,'t) m;
  delete       : 'k -> (unit,'t)m;
  detach       : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
  blk_len      : unit -> (int,'t)m;
  pcache_write : unit -> (unit,'t)m;
  pcache_sync  : unit -> (unit,'t)m;
}

  (* read_pcache       : root:'r -> read_blk_as_buf:('r -> (buf,'t)m) ->  *)
    (* ((('k,'v)kvop list * 'r option) list * 'buf,'t)m *)

(* module Pcache_state = struct *)

(* $(PIPE2SH("""sed -n '/type[ ].*pcache_state/,/^}/p' >GEN.pcache_state.ml_""")) *)
type ('r,'kvop_map) pcache_state = {
  root_ptr    : 'r;
  past_map    : 'kvop_map;
  current_ptr : 'r;
  current_map : 'kvop_map;
  buf         : buf;  (* should be the same size as a blk *)
  buf_pos     : int;
  next_ptr    : 'r option; 
  blk_len     : int;
  dirty       : bool; (* only if buf is dirty ie data changed, or next_ptr *)
}

(*
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
*)

(** Create an empty pcache state (without writing to disk) *)
let empty_pcache_state ~ptr ~empty = {
  root_ptr=ptr;
  past_map=empty;
  current_ptr=ptr;
  current_map=empty;
  buf=ba_buf_ops.buf_create (Blk_sz.to_int blk_sz_4096);
  buf_pos=0;
  next_ptr=None;
  blk_len=1;
  dirty=true
}


(** {2 Pcache factory} *)


(* $(NOTE("""It would be nice to define types in a setting where the
   type parameters were fixed (to avoid spelling them out each time),
   and then 'export' them so that their type parameters are explicit;
   but this is currently not supported by the language; something like
   local type abbrevs would also be useful. As an alternative, we use
   lots of naming of arguments and results, to avoid having to parse
   the types themselves.""")) *)

(** NOTE the following types are unreadable in odoc; refer to src code instead *)

(* $(PIPE2SH("""sed -n '/type[ ].*pcache_factory_1/,/^>/p' >GEN.pcache_factory_1.ml_""")) *)
type ('k,'v,'r,'buf,'kvop_map,'t) pcache_factory_1 = <
  read_pcache: 
    'r -> 
    ( < tl: (('k,'v)kvop list * 'r option) list;
        hd: ('r*'buf*int) 
      >,'t)m;
  (** Read the whole pcache; all tl nxt pointers are Some; O(length of pcache) *)

  read_initial_pcache_state: 
    'r -> 
    ( ('r,'kvop_map)pcache_state, 't)m;
  (** Construct pcache state via read_cache; O(n) *)

  make_pcache_ops: <
    with_state: 
      (('r,'kvop_map)pcache_state,'t) with_state -> 
      ('k,'v,'r,'kvop_map,'t) pcache_ops;
    from_root: 
      'r -> 
      ( < 
          pcache_state_ref: ('r,'kvop_map)pcache_state ref;
          with_state: (('r,'kvop_map)pcache_state,'t) with_state;
          pcache_ops: ('k,'v,'r,'kvop_map,'t) pcache_ops
        >, 't)m
  (** NOTE this constructs a [with_state] via a reference *)
  >
>

(* assume blk_alloc is given *)
(* $(PIPE2SH("""sed -n '/type[ ].*pcache_factory[ ]/,/^>/p' >GEN.pcache_factory.ml_""")) *)
type ('k,'v,'r,'buf,'kvop_map,'t) pcache_factory = <
  kvop_map_ops: ('k,('k,'v)kvop,'kvop_map) Tjr_map.map_ops;

  empty_pcache_state: ptr:'r -> ('r,'kvop_map)pcache_state;
  (** NOTE this does not access disk *)

  with_read_blk_as_buf: 
    read_blk_as_buf : ('r -> ('buf,'t) m) ->
    flush_tl        : (('r,'kvop_map)pcache_state -> (unit,'t)m) ->
    ('k,'v,'r,'buf,'kvop_map,'t) pcache_factory_1;
  (** NOTE flush_tl is used to write to disk *)

  with_blk_dev_ops: 
    blk_dev_ops : ('r,'buf,'t)blk_dev_ops -> (* NOTE this requires 'buf as 'blk *)
    ('k,'v,'r,'buf,'kvop_map,'t) pcache_factory_1;
  (** NOTE blk_dev_ops is used to provide [flush_tl] *)
>    


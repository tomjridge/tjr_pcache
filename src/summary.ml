(** Summary of main types *)

(**
{[
(** Make functor argument sig, including types and values *)
module type S = sig
  type t
  val monad_ops: t monad_ops

  type k
  type v
  type r
  type blk_id = r
  type blk

  val blk_ops: (blk,buf) blk_ops
  val buf_ops: buf buf_ops
  val k_cmp: k -> k -> int

  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
  val r_mshlr: r bp_mshlr

end

  type ('k,'v,'r,'kvop_map) detach_info = { 
    root_ptr    : 'r;
    past_map    : 'kvop_map;
    current_ptr : 'r;
    current_map : 'kvop_map;
  }

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

  type ('k,'v,'r,'kvop_map,'t) pcache_ops = {
    find         : 'k -> ('v option,'t) m;
    insert       : 'k -> 'v -> (unit,'t) m;
    delete       : 'k -> (unit,'t)m;
    detach       : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
    blk_len      : unit -> (int,'t)m;
    pcache_write : unit -> (unit,'t)m;
    pcache_sync  : unit -> (unit,'t)m;
  }

  type ('r,'kvop_map) pcache_state = {
    root_ptr    : 'r;
    past_map    : 'kvop_map;
    current_ptr : 'r;
    current_map : 'kvop_map;
    buf         : ba_buf;  (* should be the same size as a blk *)
    buf_pos     : int;
    next_ptr    : 'r option; 
    blk_len     : int;
    dirty       : bool; (* only if buf is dirty ie data changed, or next_ptr *)
  }

]}
*)

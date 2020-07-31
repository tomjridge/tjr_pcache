(** Summary *)

(**

The persistent cache is an on-disk singly-linked list of blks (each
   blk contains the data, and an optional next pointer). The list
   starts from a "root" block. The data consists of a list of map
   operations (insert or delete), which is effectively a log of
   operations that have taken place on the in-memory map. New
   operations are appended to the end of the list (multiple
   insert/delete operations per blk). The on-disk list grows
   automatically: new blks are appended when the current "tail"
   becomes full.

In memory we maintain the map itself.

At a given point, we can execute one of the (find,insert,delete)
   oeprations.

Additionally, we can perform a "detach" operation. This effectively
   dicards everything upto the current tail block. The result returned
   from the detach operation is the "past map" (everything from the
   old root upto -- but not including -- the current tail block). The
   new list starts with the current tail block.

The idea is that modifications to the persistent cache are quick
   (typically one block write). So we can use it in front of an
   on-disk B-tree. However, the persistent cache requires an O(n)
   initialization (all blocks in the list need to be read). So
   periodically we need to detach the cache, and merge into the
   B-tree.

The persistent cache is similar to the "write ahead log" found in
   traditional filesystems, but we have one per filesystem object.

Main types:

{[   type ('k,'v,'r,'kvop_map) detach_info = { 
    root_ptr    : 'r;
    past_map    : 'kvop_map;
    current_ptr : 'r;
    current_map : 'kvop_map;
  }

type ('a,'k,'v,'r,'buf,'kvop_map,'t) pcache_factory = <
  (* NOTE the type exposed by this library is slightly different than
     the above; see the package module Tjr_pcache *)

  note_these_types_are_equal : 'a -> ('k,'v)kvop -> unit;

  kvop_map_ops : ('k,('k,'v)kvop,'kvop_map) Tjr_map.map_ops;

  empty_pcache : 'r -> ('r,'kvop_map) pcache_state;
  (** NOTE just in memory; doesn't write to disk *)

  simple_plist_factory : ('a,'r,'buf,'buf,'t) simple_plist_factory;
  (* NOTE in our use cases, 'buf='blk *)

  plist_to_pcache : 
    simple_plist_ops : ('a,'r,'t)simple_plist_ops -> 
    with_state       : (('r,'kvop_map) pcache_state,'t) with_state ->
    ('k,'v,'r,'kvop_map,'t) pcache_ops;
  (** NOTE you have to construct the plist first using the
     simple_plist_factory, then convert to pcache *)


  with_ : 
    blk_dev_ops  : ('r,'buf,'t)blk_dev_ops ->
    barrier      : (unit -> (unit,'t)m) -> 
    freelist_ops : ('r,'t) freelist_ops_af -> 
    <
      create  : unit -> (('k,'v,'r,'kvop_map,'t) pcache_ops,'t)m;
      restore : hd:'r -> (('k,'v,'r,'kvop_map,'t) pcache_ops,'t)m;
      (* NOTE for restore, we only need the hd ptr *)
    >;

>

  type ('k,'v,'r,'kvop_map,'t) pcache_ops = {
    find         : 'k -> ('v option,'t) m;
    insert       : 'k -> 'v -> (unit,'t) m;
    delete       : 'k -> (unit,'t)m;
    detach       : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
    blk_len      : unit -> (int,'t)m;
    pcache_sync  : unit -> (unit,'t)m;
  }

  type ('r,'kvop_map) pcache_state = {
    root_ptr    : 'r;
    past_map    : 'kvop_map;
    current_ptr : 'r;
    current_map : 'kvop_map;
  }
 ]}

*)

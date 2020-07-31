(** Main pcache interfaces; don't open (defines buf and buf_ops) *)

open Kvop

let buf_ops = ba_buf_ops

module Detach_info = struct
  (** The result of "detaching" the map. We get the abstract map for
      all but the current node, and information about the current
      node. *)
  (* $(PIPE2SH("""sed -n '/type[ ].*detach_info = /,/}/p' >GEN.detach_info.ml_""")) *)
  type ('k,'v,'r,'kvop_map) detach_info = { 
    root_ptr    : 'r;
    past_map    : 'kvop_map;
    current_ptr : 'r;
    current_map : 'kvop_map;
  }
  type ('k,'v,'r,'kvop_map) t = ('k,'v,'r,'kvop_map) detach_info
end
include Detach_info


module Pcache_ops = struct
  (** For the detach operation, we get the map upto the current node,
      and the map for the current node. NOTE if root_ptr = current_ptr,
      then nothing was detached. *)
  (* $(PIPE2SH("""sed -n '/type[ ].*pcache_ops/,/}/p' >GEN.pcache_ops.ml_""")) *)
  type ('k,'v,'r,'kvop_map,'t) pcache_ops = {
    find         : 'k -> ('v option,'t) m;
    insert       : 'k -> 'v -> (unit,'t) m;
    delete       : 'k -> (unit,'t)m;
    detach       : unit -> ( ('k,'v,'r,'kvop_map) detach_info, 't) m;
    blk_len      : unit -> (int,'t)m;
    pcache_sync  : unit -> (unit,'t)m;
  }
  (* $(FIXME("barrier? taken care of by plist?")) *)
end
include Pcache_ops


module Pcache_state = struct
  (** Pcache state, in addition to the plist *)
  (* $(PIPE2SH("""sed -n '/type[ ].*pcache_state = {/,/}/p' >GEN.pcache_state.ml_""")) *)
  type ('r,'kvop_map) pcache_state = {
    root_ptr    : 'r;
    past_map    : 'kvop_map;
    current_ptr : 'r;
    current_map : 'kvop_map;
  }
  type ('r,'kvop_map) t = ('r,'kvop_map) pcache_state 
end
include Pcache_state


(** Create an empty pcache state (without writing to disk) *)
let empty_pcache_state ~ptr ~empty = {
  root_ptr=ptr;
  past_map=empty;
  current_ptr=ptr;
  current_map=empty;
}


(** {2 Pcache factory} *)

(** We implement on top of the plist factory. Thus, initialization
   uses the plist routines. What is left is to implement the
   pcache-specific functionality.

*)


(* $(NOTE("""It would be nice to define types in a setting where the
   type parameters were fixed (to avoid spelling them out each time),
   and then 'export' them so that their type parameters are explicit;
   but this is currently not supported by the language; something like
   local type abbrevs would also be useful. As an alternative, we use
   lots of naming of arguments and results, to avoid having to parse
   the types themselves.""")) *)

(** NOTE the following types are unreadable in odoc; refer to src code
   instead *)



(* assume blk_alloc is given *)
(* $(PIPE2SH("""sed -n '/type[ ].*pcache_factory[ ]/,/^>/p' >GEN.pcache_factory.ml_""")) *)
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

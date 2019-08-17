
(* FIXME use fs_shared
module Blk_dev_ops = struct
  type ('blk_id,'blk,'dev,'t) blk_dev_ops = {
    write:
      dev:'dev -> 
      blk_id:'blk_id -> 
      blk:'blk -> 
      (unit,'t) m;
    read:
      dev:'dev -> 
      blk_id:'blk_id -> 
      ('blk,'t) m;
  }
end
include Blk_dev_ops
*)

(*
module Ins_del_op = struct
  (** A concrete type for insert and delete operations *)

  (** An op is either insert or delete. These are the entries that get
      written to disk. *)

  (* FIXME now in fs_shared? *)

  [@@@ocaml.warning "-39"]
  type ('k,'v) op = Insert of 'k * 'v | Delete of 'k [@@deriving bin_io, yojson]

  let op2k = function
    | Insert (k,_v) -> k
    | Delete k -> k
end
include Ins_del_op
*)

module Ins_del_op = Tjr_fs_shared.Kv_op


module Map_ops = struct

  type ('k,'v,'t) map_ops = {
    find: 'k -> ('v option,'t) m;
    insert: 'k -> 'v -> (unit,'t) m;
    delete: 'k -> (unit,'t)m;
  }

end
(* NOTE don't include *)


module Pl_types = struct


  (** A persistent list maintains a state, iso to a pair of
  (data,next_ptr option). It knows nothing about the data (especially,
  it does not know the marshalled size of the data). It allows to set
  the "current" data (possibly many times), to step to a new node (a
  node corresponds to a block), and to issue a write to disk. *)

  (** The persistent list state. Each node consists of data and a
      possible next pointer (initially None, but may be set
      subsequently). For [new_node], the ptr is the ptr of the new block,
      the second argument is the data, and the third is the internal repr. of the current node.


      NOTE the type ['a] is the type of the data stored in each node.

      Type variables:

      - ['a] the type of data stored in pl nodes
      - ['ptr] the type of pointers to nodes; each node has an optional next pointer
      - ['i] the internal state of the persistent list FIXME rename?

      NOTE the module {!Simple_pl_and_pcl_implementations} contains
      the obvious implementation of this type.

  *)
  type ('a,'ptr,'i) pl_state_ops = {
    set_data: 'a -> 'i -> 'i;
    set_next: 'ptr -> 'i -> 'i;
    (* get_data: 'i -> 'a; *)
    new_node: 'ptr -> 'a -> 'i -> 'i;  (* ptr is typically stored as the "current" ptr in 'i *)
  }


  (* FIXME rename plist *)

  (** The operations provided by the persistent list. 

      - [replace_last]
        replaces the contents of the last element of the list. 
      - [new_node]
        allocates a new node at the end of the list and makes it the
        "current" node. 
      - [pl_write]
        issues a disk write for hte current node
      - [pl_sync] force a write to disk; a disk write happens automatically when allocating a new node;
        this forces a possibly-partial node to disk

  *)
  type ('a,'ptr,'t) pl_ops = {
    replace_last : 'a -> (unit,'t) m;
    new_node     : 'a -> ('ptr,'t) m;  (* NOTE we return the ptr to the new node *)
    pl_write     : unit -> (unit,'t) m;
    pl_sync      : unit -> (unit,'t) m; 
  }
end
(* NOTE don't include *)


module Pcl_types = struct

  (** A persistent chunked list interface. The state consists of something of type ['i] isomorphic to a list of elements. There is a function to convert the list to pl-format data. The cons list operation "knows" something about the marshalled size of the list, and can indicate when the size of the list has become larger than the backing node/block can handle. *)

  (** Pure interface for manipulating the [pcl_state]. Type vars:

      - ['i] the internal pcl state type (kept abstract)
      - ['e] the non-marshalled element type
      - ['pl_data] the type of data stored in a persistent list node (there is also a pointer in the pl node)


      Functions:

      - [nil] the empty state corresponding to a new node created when the old node is full; use pl_data to get the underlying pl_node
      - [snoc] to add an element
      - [pl_data] to project from the pcl state to the actual data to be written (using pl)


      NOTE that the "next" pointer manipulation has been confined to the persistent list interface.

  *)
  type ('pl_data,'e,'i) pcl_state_ops = {
    nil     :unit -> 'i;
    snoc    :'i -> 'e -> [ `Error_too_large | `Ok of 'i ];
    pl_data :'i -> 'pl_data
  }


  (** NOTE that if we store the list of e in the pcl_state, then
     pl_data potentially repeatedly marshals the prefix of the list
     that was marshalled last time; to avoid this, pcl likely stores
     the marshalled version of the e list, which is passed directly to
     pl (which then adds the next ptr). See the dmap_example in
     package tjr_pcache_example for an example. *)


  (* pcl_ops ---------------------------------------------------------- *)

  (** A type that records whether an element was inserted in the current
      node, or whether a new node was allocated to hold the element. *)
  type 'ptr inserted_type = 
      Inserted_in_current_node | Inserted_in_new_node of 'ptr

  (** The interface exposed by the persistent chunked list, a single
      [insert] function. NOTE how 'pl_data and 'i have disappeared. *)
  type ('e,'ptr,'t) pcl_ops = {
    insert:'e -> ('ptr inserted_type,'t) m;
    pcl_write: unit -> (unit,'t) m;
    pcl_sync: unit -> (unit,'t) m;
  }

end
(* don't include *)



module Dcl_types = struct
  (** A "detachable chunked list". This is like the persistent chunked
      list, except that we provide a "detach" operation which drops the
      tail of the list.

  *)

  (** Operations on the "abstract" state. The pcl state is something
      like a list of map operations. The 'abs type is something like
      the "map" view of these operations (needed because the list is
      redundant, or at the very least inefficient for map
      operations). Type vars:


      - ['op] is eg insert(k,v), delete(k) - 'abs is the "abstraction"

      Operations:

      - empty, the empty map

      - add, to add an operation to the abstract state

      - merge, to merge two maps (second takes precedence); used when
        a new node is created, and the old node is merged into the
        accumulated past nodes.


  *)
  type ('op,'abs) abs_ops = {
    empty: 'abs;
    add: 'op -> 'abs -> 'abs;
    merge: 'abs -> 'abs -> 'abs;  (* second arg takes precedence *)
  }

  let abs_singleton ~abs_ops op = abs_ops.add op abs_ops.empty [@@inline]



  (* dcl state -------------------------------------------------------- *)

  (** The state of the DCL. Fields are:

      - [start_block] is the root of the log
      - [current_block] is the current block being written to
      - [abs_past] is the abstract view of ops from root to just before [current_block]
      - [abs_current] is the abstract view of ops for the current block

      NOTE unlike Pl and Pcl, we have a concrete type for the state, since
      we don't expect to have any extra info stored at this point. (FIXME what about dcl_dummy_implementation where we need to store all ptrs?) But
      perhaps we can avoid some of these extra type params if we keep dcl
      state abstract as ['dcl_state]. But this seems unlikely.

      NOTE [block_list_length]: this is the number of blocks from the
      underlying chunked list, used to store the ops (not the
      abstract representation!)

      NOTE upto this point, the pl and the pcl have not explicitly tracked the start of the list. FIXME perhaps they should? This has advantages in that the abstraction is self-contained.

  *)
  type ('ptr,'abs) dcl_state = {
    start_block       : 'ptr;  
    current_block     : 'ptr;
    block_list_length : int;
    abs_past          : 'abs;  
    abs_current       : 'abs;
  }


  (** The DCL operations: 

      - [add] to add an op; does not necessarily write to disk
      - [peek] to reveal the dcl_state (FIXME why?)
      - [detach] to issue a detach operation (eg prior to rolling the past entries into a B-tree)
      - [block_list_length] to provide information to help determine when to roll up

      The [detach] operation means that we should start a new cache from the
      current block. 

      The return result is the ptr and map corresponding
      to the contents of everything up to the current block, and the ptr
      and map for the current block. The intention is that the detached
      part is then rolled into the B-tree. If we only have 1 block, then
      nothing is rolled up. This occurs when [old_ptr] is the same as
      [new_ptr] FIXME use a new type 

      NOTE detach returns the dcl_state since this includes at least all the fields we need.
  *)
  type ('op,'abs,'ptr,'t) dcl_ops = {
    add               : 'op -> (unit,'t) m;     (* NOTE add rather than insert, to avoid confusion *)
    peek              : unit -> (('ptr,'abs)dcl_state,'t) m;    
    detach            : unit -> (('ptr,'abs)dcl_state, 't) m;
    block_list_length : unit -> (int,'t) m;
    dcl_write         : unit -> (unit,'t) m;
    dcl_sync          : unit -> (unit,'t) m;
  }

end



module Dmap_types = struct
  (** A dmap is effectively just a DCL with a refined 'op type and 'abs
      type. However, we also include functionality to convert to a
      standard map interface (not one based on ops). *)


  open Dcl_types


  (** NOTE dmap_state is just an abbreviation for dcl_state *)
  type ('ptr,'k,'v) dmap_state = 
    ('ptr,
     ('k,'v)kvop_map) dcl_state

  (** NOTE dmap_dcl_ops is just an abbreviation for dcl_ops with:

      - 'op the type of kv op
      - 'abs the type of kv op_map

  *)
  type ('ptr,'k,'v,'t) dmap_dcl_ops = 
    (('k,'v) kvop, 
     ('k,'v) kvop_map,
     'ptr,
     't) dcl_ops

  (** The result of "detaching" the map. We get the abstract map for
     all but the current node, and information about the current
     node. FIXME could be part of dcl_ops *)
  type ('k,'v,'ptr) detach_info = { 
    past_map    : ('k,'v) kvop_map;
    current_map : ('k,'v) kvop_map;
    current_ptr : 'ptr
  }

  (** For the detach operation, we get the map upto the current node,
      and the map for the current node *)
  type ('k,'v,'ptr,'t) dmap_ops = {
    find              : 'k -> ('v option,'t) m;
    insert            : 'k -> 'v -> (unit,'t) m;
    delete            : 'k -> (unit,'t)m;
    detach            : unit -> ( ('k,'v,'ptr) detach_info, 't) m;
    block_list_length : unit -> (int,'t)m;
    dmap_write        : unit -> (unit,'t)m;
    dmap_sync         : unit -> (unit,'t)m
  }



end

(*
module Pcache_layers = struct
  open Pl_types
  open Pcl_types
  open Dmap_types

  (** NOTE in the following type, the option refs have to be
     initialized before calling dmap_ops *)
  type ('k,'v,'ptr,'t,'pl_data,'pl_internal_state,'pcl_internal_state) pcache_layers = {
    monad_ops: 't monad_ops;
    pl_state_ops: ('pl_data,'ptr,'pl_internal_state)pl_state_ops;
    pcl_state_ops: ('pl_data,('k,'v)op,'pcl_internal_state)pcl_state_ops;
    alloc:(unit -> ('ptr,'t)m)option ref;
    with_pl:('pl_internal_state,'t) with_state option ref;
    with_pcl:('pcl_internal_state,'t) with_state option ref;
    with_dmap:(('ptr,'k,'v)dmap_state,'t) with_state option ref;
    dmap_ops: write_node:('pl_internal_state -> (unit,'t)m) ->  ('k,'v,'ptr,'t)dmap_ops
  }

end
*)

(* NOTE use merlin to get the collection of all types, without ocamldoc *)
module Internal_collection_of_all_types = struct
  include Map_ops
  include Pl_types
  include Pcl_types
  include Dcl_types
  include Dmap_types
end

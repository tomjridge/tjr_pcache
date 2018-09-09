(* -*- org -*- *)

(** Main documentation entry point for [tjr_pcache] *)

(**

{1 Introduction}

This library implements a persistent cache (pcache), i.e., an on-disk
cache. This should be used with [tjr_btree] to reduce the writes going
to the B-tree.

{1 Ancestor projects}

This library depends on [tjr_btree] (the GOM -- module {!Gom} -- is
built on top of the B-tree and the pcache).

{1 Persistent list }

The module {!Persistent_list} implements a persistent list. This is a
singly-linked on-disk list. The main function is
{!Persistent_list.make_persistent_list}.

{1 Persistent chunked list}

The module {!Persistent_chunked_list} refines persistent list, by
exposing that multiple items may be stored in a single block.

{1 Persistent cache (log) }

The module {!Persistent_log} provides the functionality we need from
the persistent cache, including the detach operation. The main
function is {!Persistent_log.make_plog_ops}.

{1 The GOM}

The Global Object Map (GOM) is essentially a B-tree with a persistent
cache (module {!Gom}).

*)


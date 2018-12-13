(* -*- org -*- *)

(** Main documentation entry point for [tjr_pcache] *)

(**

{1 Introduction}

This library implements a persistent cache (pcache), i.e., an on-disk
cache. This should be used with [tjr_btree] to reduce the writes going
to the B-tree.

The main modules are included below.

*)


include Persistent_list

include Persistent_chunked_list

include Detachable_chunked_list

include Detachable_map

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

Main types:

{[ $(INCLUDE("GEN.*.ml_")) ]}

*)

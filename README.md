# `tjr_pcache`

This is a persistent cache, part of ImpFS. It maintains a persistent
list of map operations, which avoids actually executing these
operations against the B-tree.

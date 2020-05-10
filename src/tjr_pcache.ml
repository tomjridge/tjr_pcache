(** Persistent cache functionality. *)

module Pcache_intf = Pcache_intf

type ('k,'v,'r,'kvop_map,'t) pcache_ops = ('k,'v,'r,'kvop_map,'t) Pcache_intf.pcache_ops

module Make = Make

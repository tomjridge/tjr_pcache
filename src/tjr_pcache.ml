(** Core persistent cache functionality; don't use these interfaces -
   instead, use the versions in the examples package. *)

module Pcache_intf = Pcache_intf

type ('k,'v,'r,'kvop_map,'t) dmap_ops = ('k,'v,'r,'kvop_map,'t) Pcache_intf.dmap_ops


module Make_ = Make_

module Make = Make_.Make

module Make_with_fixed_types = Make_.Make_with_fixed_types

let make x : ('k,'v,'r,'kvop_map,'t) dmap_ops = Make_.make x




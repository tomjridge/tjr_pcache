(** Core persistent cache functionality. *)

(** {b NOTE!!! don't use these interfaces -
   instead, use the versions in the pcache examples package.} *)

module Pcache_intf = Pcache_intf

type ('k,'v,'r,'kvop_map,'t) dmap_ops = ('k,'v,'r,'kvop_map,'t) Pcache_intf.dmap_ops


(* module Pvt_make = Pvt_make *)

module Make = Pvt_make.Make

module Make_with_fixed_types = Pvt_make.Make_with_fixed_types

let make x : ('k,'v,'r,'kvop_map,'t) dmap_ops = Pvt_make.make x




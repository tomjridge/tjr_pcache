(** Core persistent cache functionality. *)

(** {b NOTE!!! don't use these interfaces -
   instead, use the versions in the pcache examples package.} *)

module Pcache_intf = Pcache_intf
(* module Pcache_state = Pcache_intf.Pcache_state *)

type ('k,'v,'r,'kvop_map,'t) pcache_ops = ('k,'v,'r,'kvop_map,'t) Pcache_intf.pcache_ops


(* module Pvt_make = Pvt_make *)

module Make = Make

(* module Make_with_fixed_types = Pvt_make.Make_with_fixed_types *)

(* let make x : ('k,'v,'r,'kvop_map,'t) pcache_ops = Pvt_make.make x *)




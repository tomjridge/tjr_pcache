(** Persistent cache functionality. *)

module Pcache_intf = Pcache_intf

type ('k,'v,'r,'kvop_map,'t) pcache_ops = ('k,'v,'r,'kvop_map,'t) Pcache_intf.pcache_ops

module Make = Make


(**

{[

$(INCLUDE("GEN.S.ml_"))

$(INCLUDE("GEN.pcache_factory_1.ml_"))

$(INCLUDE("GEN.pcache_factory.ml_"))

]}


*)

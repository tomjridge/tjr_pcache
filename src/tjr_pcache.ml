(** Persistent cache functionality. *)

include Summary

module Pcache_intf = Pcache_intf
open Pcache_intf

module Detach_info = Detach_info

module Pcache_ops = Pcache_ops

module Pcache_state = Pcache_state

let empty_pcache_state = empty_pcache_state

type ('k,'v,'r,'kvop_map,'t) pcache_ops = ('k,'v,'r,'kvop_map,'t) Pcache_ops.pcache_ops

(** NOTE this is slightly different to the internal pcache_factory -
   the 'a is revealed as ('k,'v)kvop *)
type ('k,'v,'r,'buf,'kvop_map,'t) pcache_factory = 
  (('k,'v)kvop,'k,'v,'r,'buf,'kvop_map,'t) Pcache_intf.pcache_factory

module Make = Make

let pcache_examples = Make.examples

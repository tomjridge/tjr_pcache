(** Pcache example *)

module Pcache_example_intf = Pcache_example_intf
module Intf = Pcache_example_intf

(* FIXME rename to pcache_example *)
module Dmap_example = Dmap_example

module Make_layers = Dmap_example.Make_layers

module Common_instances = struct  
  module With_lwt = Make_layers.With_lwt
end

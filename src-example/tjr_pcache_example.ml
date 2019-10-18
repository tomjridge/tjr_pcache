(** Pcache example *)

open Make_

module Make_with_fixed_types = Make_with_fixed_types

module type MC' = MC with type k = int and type v = int and type r = int

module Int_int_marshalling_config : MC' = struct
  open Bin_prot.Std
  type k = int [@@deriving bin_io]
  type v = int [@@deriving bin_io]  
  type r = int [@@deriving bin_io]
  type kvop = (k,v)Kvop.kvop [@@deriving bin_io]

  (** This is the max # of bytes required for k *)
  let k_size = 9
  let v_size = 9
  let r_size = 9  
end

let int_int_marshalling_config = (module Int_int_marshalling_config : MC')

type tjr_pcache_example_map

let make_map_ops cmp : ('a, 'b, ('a, 'b, tjr_pcache_example_map) Tjr_map.map) map_ops =
  let map_ops =  Tjr_map.make_map_ops cmp in
  let merge ~older ~newer = Tjr_map.map_merge ~map_ops ~old:older ~new_:newer in
  let Tjr_map.{ empty; find_opt; add; remove; _ } = map_ops in
  { empty; find_opt; insert=add; delete=remove; merge }

let make ~blk_ops ~blk_alloc ~with_dmap ~write_to_disk = 
  let map_ops = make_map_ops Pervasives.compare in
  let x = { 
    monad_ops=lwt_monad_ops;
    marshalling_config=int_int_marshalling_config;
    map_ops;
    blk_ops;
    blk_alloc;
    with_dmap;
    write_to_disk
  }
  in
  let y = Make_.make x in
  (x,y)

let _ :
blk_ops:'a blk_ops ->
blk_alloc:(unit -> (int, lwt) m) ->
with_dmap:((int, (int, int, tjr_pcache_example_map) Tjr_map.map) dmap_state,
           lwt)
          with_state ->
write_to_disk:((int, (int, int, tjr_pcache_example_map) Tjr_map.map)
               dmap_state -> (unit, lwt) m) ->
(lwt monad_ops, (module MC'),
 (int, int, (int, int, tjr_pcache_example_map) Tjr_map.map) map_ops,
 'a blk_ops, unit -> (int, lwt) m,
 ((int, (int, int, tjr_pcache_example_map) Tjr_map.map) dmap_state, lwt)
 with_state,
 (int, (int, int, tjr_pcache_example_map) Tjr_map.map) dmap_state ->
 (unit, lwt) m)
c_dmap *
(int, int, int, (int, int, tjr_pcache_example_map) Tjr_map.map, lwt) dmap_ops
 = make

open Tjr_monad.Types
open Dcl_types

type ('ptr,'k,'v) dmap_state = ('ptr,('k,'v)Tjr_polymap.t) dcl_state

type ('ptr,'k,'v,'t) dmap_ops = 
  (('k,'v) Ins_del_op_type.op, 
   ('k,'v)Tjr_polymap.t,
   'ptr,
   't) dcl_ops

(** For the detach operation, we get the map upto the current node,
   and the map for the current node *)
type ('k,'v,'t) dmap_as_map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t)m;
  detach: unit -> ((('k,'v)Tjr_polymap.t * ('k,'v)Tjr_polymap.t),'t)m;
}

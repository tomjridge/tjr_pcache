(** An example use of the detachable chunked list, for an on-disk
   persistent cache, with key type int, value type int *)

open Ins_del_op_type
open Pcl_types
(* open Dcl_types *)

(* repr ------------------------------------------------------------ *)

(** For this example, we use simple marshalling based on OCaml's
   inbuilt module *)

(** Each node is stored in a block on disk. Each node contains a list
   of kv op. We handle marshalling in the write_node parameter *)

let blk_sz=4096  (* FIXME *)

type ('k,'v,'bytes) repr' = {
  repr_ops:('k,'v) op list;
  repr_bytes:'bytes; (* padded? *)
}

module Internal = struct

  let repr_ops () : (('k,'v) op,  ('k,'v,'bytes) repr') repr_ops = {
    nil={ repr_ops=[];
          repr_bytes=Marshal.to_string [] []
        };
    snoc=(fun e repr -> 
        (* attempt to add another int, and check byte size *)
        let new_repr = 
          let repr_ops = repr.repr_ops@[e] in
          { repr_ops;
            repr_bytes=Marshal.to_string repr_ops []
          }
        in
        match String.length new_repr.repr_bytes with 
        | x when x>blk_sz -> `Error_too_large
        | _ -> `Ok new_repr);
    repr_to_list=(fun repr -> repr.repr_ops);
  }

  (** NOTE as Pcl, but with refined repr_ops *)
  let make_pcl_ops' ~monad_ops ~pl_ops ~with_pcl = 
    Persistent_chunked_list.make_pcl_ops 
      ~monad_ops 
      ~pl_ops 
      ~repr_ops:(repr_ops ())
      ~with_pcl


  let make_dcl_ops'
      ~monad_ops
      ~write_node
      ~alloc
      ~(with_pl:(('ptr, ('k, 'v, string) repr') Pl_types.pl_state, 't)
            Tjr_monad.With_state.with_state)
      ~with_pcl
      ~with_dcl
    =
    let pl_ops = 
      Persistent_list.make_persistent_list
        ~monad_ops ~write_node ~alloc ~with_pl
    in
    let pcl_ops = make_pcl_ops' ~monad_ops ~pl_ops ~with_pcl in
    let dcl_ops = 
      Detachable_chunked_list.make_dcl_ops 
        ~monad_ops ~pcl_ops ~with_dcl
    in
    dcl_ops

end

(* explicit type to force tyvar naming in doc *)
let make_dcl_ops'  :    
    monad_ops:'t Tjr_monad.Monad_ops.monad_ops ->
    write_node:('ptr ->
                ('ptr, ('k, 'v, string) repr') Pl_types.pl_node ->
                (unit, 't) Tjr_monad.Monad_ops.m) ->
    alloc:(unit -> ('ptr, 't) Tjr_monad.Monad_ops.m) ->
    with_pl:(('ptr, ('k, 'v, string) repr') Pl_types.pl_state, 't)
      Tjr_monad.With_state.with_state ->
    with_pcl:((('k, 'v) op, ('k, 'v, string) repr') pcl_state, 't)
      Tjr_monad.With_state.with_state ->
    with_dcl:((('k, ('k, 'v) op) Tjr_polymap.t, 'ptr) Dcl_types.dcl_state, 't)
      Tjr_monad.With_state.with_state ->
    ('k, 'v, ('k, ('k, 'v) op) Tjr_polymap.t, 'ptr, 't) Dcl_types.dcl_ops
  = Internal.make_dcl_ops'

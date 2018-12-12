(** Spec of dmap - just a map *)

(* open Tjr_monad.Types *)
open Tjr_store
open Store_passing

let make_store_passing_map ~store  =
  let map_ops = Ins_del_op_type.default_kvop_map_ops () in
  mk_ref map_ops.map_empty store |> fun (s,map_ref) ->
  let with_spec f = with_ref map_ref f in
  let find k = with_spec (fun ~state:map ~set_state ->
      map_ops.map_find k map |> fun vopt ->
      return vopt)
  in
  let insert k v = with_spec (fun ~state:map ~set_state ->
      map_ops.map_add k v map |> fun map ->
      set_state map)
  in
  let delete k = with_spec (fun ~state:map ~set_state ->
      map_ops.map_remove k map |> fun map ->
      set_state map)
  in
  Map_ops.{find;insert;delete}
  

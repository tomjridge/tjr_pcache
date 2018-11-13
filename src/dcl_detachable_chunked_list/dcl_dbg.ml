(** Debug support for DCL *)

open Tjr_monad.Types
open Detachable_chunked_list

(* use the pclist_to_nodes debug function *)

(* The debug state is a pair of lists representing the past and
   current maps (the lists are derived from the on-disk contents). *)
(*
type ('k,'v) dbg = {
  dbg_current: ('k,'v) op list;
  dbg_past: ('k,'v) op list
} 
*)

(* specialize for yojson *)
let op_to_yojson a b op : Yojson.Safe.json = match op with
    Insert(k,v) -> `String (Printf.sprintf "Insert(%d,%d)" k v)
  | Delete k -> `String (Printf.sprintf "Delete(%d)" k)

let op_of_yojson a b op = failwith __LOC__

type find_result = (int,int) op option [@@deriving yojson]

type dbg = {
  dbg_current: (int,int) op list;
  dbg_past: (int,int) op list
} [@@deriving yojson]

let init_dbg = {
  dbg_current=[];
  dbg_past=[]
}

(* note this has a separate ptr FIXME needed? *)
let plog_to_dbg ~pclist_to_nodes ~get_plog_state (s:'t) : (* ('k,'v) *) dbg =
  let plog_state = s|>get_plog_state in
  pclist_to_nodes ~ptr:plog_state.start_block s
  |> List.map (fun (ptr,es) -> es)
  |> fun ess ->
  match plog_state.start_block = plog_state.current_block with
  | true -> 
    assert(List.length ess=1); (* ptr is s.start_block *)
    let dbg_current=List.concat ess in
    (* FIXME are we sure dbg_current is wellformed? *)
    {dbg_past=[]; dbg_current}
  | false -> 
    assert(ess <> []);
    {dbg_past=(Tjr_list.butlast ess |> List.concat); dbg_current=(Tjr_list.last ess) }

let _ = plog_to_dbg


(* for an association list, we need new entries to be at the front *)
let dbg2list {dbg_current; dbg_past} = 
  (List.rev dbg_current @ List.rev dbg_past)

let dbg2assoc_list dbg = dbg |> dbg2list |> List.map (fun op -> (op2k op,op))

let find k dbg = 
  dbg |> dbg2assoc_list |> fun xs ->
  match List.assoc k xs with
  | exception _ -> None
  | v -> Some v 


type tmp = Yojson.Safe.json option [@@deriving yojson]

let with_world = Tjr_monad.State_passing.with_world


(* take an existing plog ops, and add testing code based on the dbg state *)
let make_checked_plog_ops ~monad_ops ~plog_ops ~plog_to_dbg ~set_dbg ~get_dbg 
  : ('k,'v,'map,'ptr,'t) plog_ops
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  let get_state () = with_world (fun s -> (s,s)) in
  let set_state s' = with_world (fun s -> ((),s')) in
  let find k = 
    get_state () >>= fun s ->
    let expected = find k (get_dbg s) in
    plog_ops.find k >>= fun v ->
    Pcache_debug.log_lazy (fun () ->
        expected |> find_result_to_yojson |> fun expected ->
        v |> find_result_to_yojson |> fun v ->
        Printf.sprintf "%s:\n    expected(%s)\n    actual(%s)"
          "make_checked_plog_ops.find"
          (expected |> Yojson.Safe.pretty_to_string)
          (v |> Yojson.Safe.pretty_to_string));
    assert(v=expected);
    return v
  in
  let add op = 
    get_state () >>= fun s ->
    plog_ops.add op >>= fun () ->
    get_state () >>= fun s' ->
    get_dbg s |> fun dbg ->
    plog_to_dbg (* ~ptr:(s'|>start_block) *) s' |> fun dbg' ->
    Pcache_debug.log_lazy (fun () ->
        Printf.sprintf "%s: %s %s"
          "make_checked_plog_ops.add"
          (dbg_to_yojson dbg |> Yojson.Safe.pretty_to_string)
          (dbg_to_yojson dbg' |> Yojson.Safe.pretty_to_string));
    assert(dbg2list dbg' = (op::(dbg2list dbg)));
    (* now need to update the dbg state *)
    set_state (s' |> set_dbg dbg') >>= fun () ->
    return ()
  in
  let detach () = 
    plog_ops.detach () >>= fun r ->
    get_state () >>= fun s' ->
    (* set dbg state *)
    plog_to_dbg (* ~ptr:(s'|>start_block) *) s' |> fun dbg' ->
    set_dbg dbg' s' |> fun s' ->
    set_state s' >>= fun () ->
    return r
  in  
  let get_block_list_length = plog_ops.get_block_list_length in
  { find; add; detach; get_block_list_length }


let _ = make_checked_plog_ops


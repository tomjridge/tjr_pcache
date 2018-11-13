(** Debug support for DCL.  The spec is expressed as an abstraction
   from the impl, with wf checks for transitions. Provide code to wrap an
   existing dcl and check that the state changes conform to the
   spec. 

    An alternative (executable spec) is in {! Dcl_spec}. 
*)

(* FIXME merge this with dcl_spec *)

open Tjr_monad.Types
open Ins_del_op_type
open Detachable_chunked_list

(* fiddle with op type ---------------------------------------------- *)

(* specialize for yojson *)
let op_to_yojson a b op : Yojson.Safe.json = match op with
    Insert(k,v) -> `String (Printf.sprintf "Insert(%d,%d)" k v)
  | Delete k -> `String (Printf.sprintf "Delete(%d)" k)

let op_of_yojson a b op = failwith __LOC__

type find_result = (int,int) op option [@@deriving yojson]


(* dbg state -------------------------------------------------------- *)

(* FIXME note that this is specialized to int -> int; generalize? *)
(** The abstract representation for the DCL spec. Essentially it is the
   list divided into the "current" block and the previous blocks (and
   each block corresponds to a list of operations).

FIXME order of lists? reversed (most recent first) - assoc lists

 *)
type dbg = {
  dbg_current: (int,int) op list;
  dbg_past: (int,int) op list
} [@@deriving yojson]


let init_dbg = {
  dbg_current=[];
  dbg_past=[]
}


(** Convert the dbg state into a simple list *)
let dbg2list {dbg_current; dbg_past} = 
  (List.rev dbg_current @ List.rev dbg_past)


(* for an association list, we need new entries to be at the front *)
let dbg2assoc_list dbg = 
  dbg |> dbg2list |> List.map (fun op -> (op2k op,op))


(* find_dbg ------------------------------------------------------------- *)

(** Implement the find operation on dbg state *)
let find k dbg = 
  dbg |> dbg2assoc_list |> fun xs ->
  match List.assoc k xs with
  | exception _ -> None
  | v -> Some v 





(* dcl to dbg ------------------------------------------------------- *)

(* note this has a separate ptr FIXME needed? *)
(** Map the DCL state to a [dbg] state *)
let dcl_to_dbg ~pclist_to_nodes ~get_dcl_state (s:'t) : (* ('k,'v) *) dbg =
  let dcl_state = s|>get_dcl_state in
  pclist_to_nodes ~ptr:dcl_state.start_block s
  |> List.map (fun (ptr,es) -> es)
  |> fun ess ->
  match dcl_state.start_block = dcl_state.current_block with
  | true -> 
    assert(List.length ess=1); (* ptr is s.start_block *)
    let dbg_current=List.concat ess in
    (* FIXME are we sure dbg_current is wellformed? *)
    {dbg_past=[]; dbg_current}
  | false -> 
    assert(ess <> []);
    { dbg_past=(Tjr_list.butlast ess |> List.concat); 
      dbg_current=(Tjr_list.last ess) }

let _ = dcl_to_dbg




(* wrap existing DCL with check ops --------------------------------- *)

(* type tmp = Yojson.Safe.json option [@@deriving yojson] *)

let with_world = Tjr_monad.State_passing.with_world

(* take an existing dcl ops, and add testing code based on the dbg state *)
let make_checked_dcl_ops ~monad_ops ~dcl_ops ~dcl_to_dbg ~set_dbg ~get_dbg 
  : ('k,'v,'map,'ptr,'t) dcl_ops
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  let get_state () = with_world (fun s -> (s,s)) in
  let set_state s' = with_world (fun s -> ((),s')) in
  let find k = 
    get_state () >>= fun s ->
    let expected = find k (get_dbg s) in
    dcl_ops.find k >>= fun v ->
    Pcache_debug.log_lazy (fun () ->
        expected |> find_result_to_yojson |> fun expected ->
        v |> find_result_to_yojson |> fun v ->
        Printf.sprintf "%s:\n    expected(%s)\n    actual(%s)"
          "make_checked_dcl_ops.find"
          (expected |> Yojson.Safe.pretty_to_string)
          (v |> Yojson.Safe.pretty_to_string));
    assert(v=expected);
    return v
  in
  let add op = 
    get_state () >>= fun s ->
    dcl_ops.add op >>= fun () ->
    get_state () >>= fun s' ->
    get_dbg s |> fun dbg ->
    dcl_to_dbg (* ~ptr:(s'|>start_block) *) s' |> fun dbg' ->
    Pcache_debug.log_lazy (fun () ->
        Printf.sprintf "%s: %s %s"
          "make_checked_dcl_ops.add"
          (dbg_to_yojson dbg |> Yojson.Safe.pretty_to_string)
          (dbg_to_yojson dbg' |> Yojson.Safe.pretty_to_string));
    assert(dbg2list dbg' = (op::(dbg2list dbg)));
    (* now need to update the dbg state *)
    set_state (s' |> set_dbg dbg') >>= fun () ->
    return ()
  in
  let detach () = 
    dcl_ops.detach () >>= fun r ->
    get_state () >>= fun s' ->
    (* set dbg state *)
    dcl_to_dbg (* ~ptr:(s'|>start_block) *) s' |> fun dbg' ->
    set_dbg dbg' s' |> fun s' ->
    set_state s' >>= fun () ->
    return r
  in  
  let undetached_block_count = dcl_ops.undetached_block_count in
  { find; add; detach; undetached_block_count }


let _ = make_checked_dcl_ops


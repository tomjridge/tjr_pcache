(** Debug support for DCL.  The spec is expressed as an abstraction
   from the impl, with wf checks for transitions. Provide code to wrap
   an existing dcl and check that the state changes conform to the
   spec.

    An alternative (executable spec) is not possible because eg we
   don't necessarily assume a fixed number of ops per block. (Perhaps
   we should... this makes the spec more restrictive, but allows for
   an executable spec)

*)

open Tjr_monad.Types
open Ins_del_op_type
open Detachable_chunked_list

let set,get = Tjr_store.(set,get)

let mk_ref' = Pl_test.mk_ref'


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

NOTE specialized to [int -> int]

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
let dcl_to_dbg 
    ~pcl_to_list
    ~(blks:'blks) 
    ~(dcl:('map,'ptr)dcl_state) 
  : dbg 
  =
  pcl_to_list ~start_block:dcl.start_block ~blks 
  (* |> List.map (fun (ptr,es) -> es)  (\* FIXME or assume pclist_to_nodes does this? *\) *)
  |> fun (ess:(int,int)op list list) ->
  match dcl.start_block = dcl.current_block with
  | true -> 
    (* NOTE only one block *)
    assert(List.length ess=1); (* ptr is s.start_block *)
    let dbg_current=List.concat ess in
    (* FIXME are we sure dbg_current is wellformed? *)
    {dbg_past=[]; dbg_current}
  | false -> 
    assert(ess <> []);
    { dbg_past=(Tjr_list.butlast ess |> List.concat); 
      dbg_current=(Tjr_list.last ess) }

let _ : 
pcl_to_list:(start_block:'ptr -> blks:'blks -> (int, int) op list list) ->
blks:'blks -> dcl:('map, 'ptr) dcl_state -> dbg
  = dcl_to_dbg


(** NOTE the dbg state is derived from the dcl state; we don't need to
   explicitly store it *)

(* wrap existing DCL with check ops --------------------------------- *)

(** Take an existing [dcl_ops], and add testing code based on the dbg state 

FIXME maybe prefer exhaustive testing rather than wrapping the monadic ops
*)
let make_checked_dcl_ops 
    ~monad_ops 
    ~dcl_ops 
    ~get_dbg
  : ('k,'v,'map,'ptr,'t) dcl_ops
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in  
  (* let with_dbg = with_dbg.with_state in *)
  let find k = 
    get_dbg () >>= fun dbg -> 
    let expected = find k dbg in
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
    get_dbg () >>= fun dbg ->
    dcl_ops.add op >>= fun () ->
    get_dbg () >>= fun dbg' ->
    Pcache_debug.log_lazy (fun () ->
        Printf.sprintf "%s: %s %s"
          "make_checked_dcl_ops.add"
          (dbg_to_yojson dbg |> Yojson.Safe.pretty_to_string)
          (dbg_to_yojson dbg' |> Yojson.Safe.pretty_to_string));
    assert(dbg2list dbg' = (op::(dbg2list dbg)));
    return ()
    (* FIXME do we need the dbg state? *)
  in
  let detach () = 
    get_dbg () >>= fun dbg ->
    dcl_ops.detach () >>= fun r ->
    get_dbg () >>= fun dbg' ->
    (* check that the transition looks reasonable FIXME *)
    return r
  in  
  let block_list_length = dcl_ops.block_list_length in
  { find; add; detach; block_list_length }



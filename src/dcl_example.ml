(** An example use of the detachable chunked list, for an on-disk
   persistent cache, with key type int, value type int *)

open Ins_del_op_type
open Pcl_types
(* open Dcl_types *)

let blk_sz=4096  (* FIXME *)


(** Each node is stored in a block on disk. Each node contains a list
   of kv op. We handle marshalling in the write_node parameter *)

type ('k,'v,'bytes) repr' = {
  repr_ops:('k,'v) op list;
  repr_bytes:'bytes; (* padded? *)
}
(** If we use Bin_prot, then ['bytes] will be a buf, with a position;
   to add an elt, we can simply write the elt at the pos, and update
   the pos and the initial "length" int.

    Alternatively we can write the elts then write a marker byte
 *)


(* node as bytes *)

module Internal0 = struct
  open Bin_prot.Std

  type node_as_bytes = 
    { ptr_opt:int option; contents: string } [@@deriving bin_io]

  let node_to_string n = 
    Bin_prot.Common.create_buf blk_sz |> fun buf ->
    bin_writer_node_as_bytes.write buf ~pos:0 n |> fun pos -> 
    assert(pos <= blk_sz);
    let s = String.init blk_sz 
        (fun i -> if i < pos then Bigarray.Array1.get buf i else ' ')
    in
    s
    
end


(* repr ------------------------------------------------------------ *)


module Internal1 = struct

  (* FIXME we may want marshaling to be aware of the next pointer in
     pl nodes *)
  let int_size_in_bytes = 8
  let int_opt_size_int_bytes = 9
  let pl_node_content_max_size = blk_sz - int_opt_size_int_bytes

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
        | x when x>pl_node_content_max_size -> `Error_too_large
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
  = Internal1.make_dcl_ops'

module Internal2 = struct
  open Tjr_store
  open Tjr_monad.With_state
  (* open Tjr_monad.State_passing *)
  open Store_passing

  (* assume monad is a tjr_store-passing monad *)
      

  type ptr = int

  (** Construct dcl ops on top of a functional store. *)
  let make_dcl_ops_with_fun_store : 
    write_node:('ptr ->
                ('ptr, ('k, 'v, string) repr') Pl_types.pl_node ->
                (unit, 't) Tjr_monad.Monad_ops.m) ->
    store:Tjr_store.t ->
    Tjr_store.t * 
    ('k, 'v, 
     ('k, ('k, 'v) op) Tjr_polymap.t, 
     'ptr, 
     't) Dcl_types.dcl_ops
    = 
    fun ~write_node ~store ->
      let repr_ops = Internal1.repr_ops () in
      let ptr0 : ptr = 0 in
      (* get some refs *)
      mk_ref ptr0 store |> fun (s,free_ref) ->
      let with_free f = with_ref free_ref f in

      let init_contents=repr_ops.nil in
      let init_node = Pl_types.{next=None;contents=init_contents} in
      let pl_state = Pl_types.{ current_ptr=ptr0; current_node=init_node } in
      mk_ref pl_state s |> fun (s,pl_ref) ->
      let with_pl f = with_ref pl_ref f in

      let elts = [] in
      let elts_repr = repr_ops.nil in
      let pcl_state = Pcl_types.{ elts; elts_repr } in
      mk_ref pcl_state s |> fun (s,pcl_ref) ->
      let with_pcl f = with_ref pcl_ref f in

      let kvop_map_ops = Ins_del_op_type.default_kvop_map_ops () in
      let empty_map = kvop_map_ops.map_empty in
      let dcl_state = Dcl_types.{
        start_block=0;
        current_block=0;
        block_list_length=1;
        map_past=empty_map;
        map_current=empty_map
      } 
      in
      mk_ref dcl_state s |> fun (s,dcl_ref) ->
      let with_dcl f = with_ref dcl_ref f in

      let alloc () = with_free (fun ~state:free ~set_state -> 
          let free = free+1 in
          set_state free >>= fun () ->
          return (free-1))
      in
      mk_ref dcl_state s |> fun (s,dcl_ref) ->
      let dcl_ops = 
        make_dcl_ops'
          ~monad_ops
          ~write_node
          ~alloc
          ~with_pl:{with_state=with_pl}
          ~with_pcl:{with_state=with_pcl}
          ~with_dcl:{with_state=with_dcl}
      in
      s,dcl_ops


  (** Construct write_node on top of a blk_dev *)

  open Blk_dev

  open Internal0

  let make_write_node ~blk_dev_ops ~dev = 
    let write_node ~(ptr:'ptr) ~(pl_node:('ptr, 'repr) Pl_types.pl_node) =
      let node_as_bytes = {ptr_opt=pl_node.next; contents=pl_node.contents.repr_bytes} in
      node_to_string node_as_bytes |> fun s ->
      (* pad to length *)
      assert (String.length s <= blk_sz);
      let s = s ^ (String.make (blk_sz - String.length s) ' ') in
      blk_dev_ops.write ~dev ~blk_id:ptr ~blk:s
    in
    let write_node ptr pl_node = write_node ~ptr ~pl_node in
    write_node

  let _ = make_write_node
    

end 

open Internal2      

let make_dcl_ops_on_file ~monad_ops ~fn = 
  let fd = Tjr_fs_shared.File_util.fd_from_file ~fn ~create:true ~init:true in
  let blk_dev_ops = 
    Blk_dev.Blk_dev_on_file.make_blk_dev_on_file ~monad_ops ~blk_sz in
  let write_node = make_write_node ~blk_dev_ops ~dev:fd in
  let store = Tjr_store.initial_store in
  let store,dcl_ops = make_dcl_ops_with_fun_store ~write_node ~store in
  fd,store,dcl_ops

open Tjr_monad.Monad_ops
open Tjr_monad.State_passing

let test_dcl_ops_on_file ~fn ~count = 
  let monad_ops : Tjr_store.t state_passing monad_ops = monad_ops () in
  let fd,store,dcl_ops = make_dcl_ops_on_file ~monad_ops ~fn in
  let s = ref store in
  List.iter
    (fun i -> 
       run ~init_state:(!s) (dcl_ops.add (Insert(i,2*i)))
       |> fun (_,s') -> s:=s')       
    (Tjr_list.from_to 1 count)
  
  

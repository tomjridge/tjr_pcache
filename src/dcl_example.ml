
(** An example use of the detachable chunked list, for an on-disk
   persistent cache, with key type int, value type int

Suppose the block size is B bytes. Then we need 10 bytes for the
   End_of_list marker.

- (B-10) bytes are available for data

When we write eg Insert(k,v), we potentially use 1+|k|+|v| bytes. We
   need to be very careful that this doesn't exceed the B-10
   limit. Particularly, if values can be largeish (eg 256 bytes) we
   need to take great care.


*)

(* open Ins_del_op_type *)



type buf = Bin_prot.Common.buf
let create_buf = Bin_prot.Common.create_buf

(* config params ---------------------------------------------------- *)

type ('k,'v,'ptr) config = {
  ptr_sz:int;
  blk_sz:int;
  k_size:int;
  v_size:int;
  k_writer: 'k Bin_prot.Type_class.writer;
  v_writer: 'v Bin_prot.Type_class.writer;
  ptr_writer: 'ptr Bin_prot.Type_class.writer;
  ptr0:'ptr;
  next_free_ptr:'ptr -> 'ptr;
}

let end_of_list_sz ~config = 1+1+config.ptr_sz   
(* End_of_list, Some/None, and int ptr; assumes binprot *)

let max_data_length ~config = config.blk_sz - (end_of_list_sz ~config)



(* list elt, like op, but with next pointer ------------------------- *)

module Elt = struct
  open Bin_prot.Std

  (** For marshalling, we have a seq of elts, where the last elt is a
      End_of_list. The empty seq is just a single elt End_of_list *)
  type ('k,'v,'ptr) elt = 
    | Insert of 'k * 'v
    | Delete of 'k 
    | End_of_list of 'ptr option
  [@@deriving bin_io]

  let elt_writer ~config = 
    bin_writer_elt config.k_writer config.v_writer config.ptr_writer
end
include Elt



(* pl instance ------------------------------------------------------ *)

(* at the pl level:
data : 'a = buf * pos
'i = buf,pos,next

at the pcl level, again we can take 

'a = buf * pos
'e = op
'i = op list as buf * pos

*)


type 'ptr pl_state = {
  data: buf * int;
  current: 'ptr;
  next: 'ptr option
}

open Blk_dev

let write_node ~config ~blk_dev_ops ~dev (pl_state: 'ptr pl_state) =
  let buf,pos = pl_state.data in
  let eol = End_of_list pl_state.next in
  let _pos = (elt_writer ~config).write buf ~pos eol in
  (* FIXME could be more efficient if we wrote direct to disk without
     going via string *)
  let blk = String.init config.blk_sz (fun i -> Bigarray.Array1.get buf i) in
  blk_dev_ops.write ~dev ~blk_id:pl_state.current ~blk

let _ = write_node

let pl_state_ops = Pl_types.{
  set_data=(fun data pl_state -> {pl_state with data});
  set_next=(fun ptr pl_state -> {pl_state with next=(Some ptr)});
  new_node=(fun current data pl_state -> { data; current; next=None })
}  
  


(* pcl -------------------------------------------------------------- *)

type pcl_state = {
  data:buf*int;
}

let pcl_state_ops ~(config:('k,'v,'ptr)config) = 
  assert(config.k_size + config.v_size+1 <= max_data_length ~config);
  Pcl_types.{
    nil=(fun () -> {data=(create_buf config.blk_sz,0)});
    snoc=(fun pcl_state (e:('k,'v)Ins_del_op_type.op) -> 
        (* remember that we have to leave 10 bytes for the
           "End_of_list" marker *)        
        let buf,pos = pcl_state.data in
        let e' = match e with
          | Insert(k,v) -> Elt.Insert(k,v)
          | Delete k -> Elt.Delete k
        in
        (* FIXME note that we need to be careful to leave enough space
           for the next ptr, and we need to ensure we can write the
           "current" elt *)
        let new_bytes_len = match e with
          | Insert _ -> config.k_size+config.v_size+1
          | Delete _ -> config.k_size+1
        in
        match pos + new_bytes_len <= max_data_length ~config with
        | true -> 
          let pos' = (elt_writer ~config).write buf ~pos e' in
          `Ok {data=(buf,pos')}
        | false -> `Error_too_large       
      ); 
    pl_data=(fun pcl_state -> pcl_state.data)
  }

let _ = pcl_state_ops


(* dcl -------------------------------------------------------------- *)

(* nothing to do *)


(* everything ------------------------------------------------------- *)


open Tjr_monad.Types
open Tjr_monad.State_passing
open Ins_del_op_type

let make_pl_ops' ~monad_ops ~write_node ~with_pl ~alloc = 
  Persistent_list.make_persistent_list
    ~monad_ops
    ~pl_state_ops
    ~write_node
    ~with_pl
    ~alloc

let _ = make_pl_ops'

let make_pcl_ops' ~monad_ops ~config ~write_node ~alloc ~with_pl ~with_pcl =
  let pl_ops = make_pl_ops' ~monad_ops ~write_node ~with_pl ~alloc in
  Persistent_chunked_list.make_pcl_ops
    ~monad_ops ~pl_ops ~pcl_state_ops:(pcl_state_ops ~config) ~with_pcl


let make_dcl_ops' 
    ~(monad_ops:'t monad_ops) 
    ~(config:('k,'v,'ptr)config) 
    ~(write_node:('ptr pl_state -> (unit,'t)m))
    ~alloc 
    ~with_pl 
    ~with_pcl 
    ~with_dcl 
  =
  let pcl_ops = 
    make_pcl_ops' ~monad_ops ~config ~write_node ~alloc ~with_pl ~with_pcl 
  in
  Detachable_chunked_list.make_dcl_ops
    ~monad_ops
    ~pcl_ops
    ~with_dcl

let _ :
monad_ops:'t monad_ops ->
config:('k, 'v, 'ptr) config ->
write_node:('ptr pl_state -> (unit, 't) m) ->
alloc:(unit -> ('ptr, 't) m) ->
with_pl:('ptr pl_state, 't) with_state ->
with_pcl:(pcl_state, 't) with_state ->
with_dcl:((('k, ('k, 'v) op) Tjr_polymap.t, 'ptr) Dcl_types.dcl_state, 't)
         with_state ->
('k, 'v, ('k, ('k, 'v) op) Tjr_polymap.t, 'ptr, 't) Dcl_types.dcl_ops
= make_dcl_ops'

let monad_ops : Tjr_store.t state_passing monad_ops = monad_ops ()

open Store_passing


(* with fun store --------------------------------------------------- *)

(** Construct dcl ops on top of a functional store. *)
let make_dcl_ops_with_fun_store 
    ~(config:('k,'v,'ptr)config) 
    ~write_node 
    ~store 
  =
  let open Tjr_store in
  (* get some refs *)
  let ptr0 = config.ptr0 in
  mk_ref ptr0 store |> fun (s,free_ref) ->
  let with_free f = with_ref free_ref f in

  let data = (Bin_prot.Common.create_buf config.blk_sz,0) in
  let pl_state = { 
    data;
    current=ptr0;
    next=None }
  in
  mk_ref pl_state s |> fun (s,pl_ref) ->
  let with_pl f = with_ref pl_ref f in

  let pcl_state = { data } in
  mk_ref pcl_state s |> fun (s,pcl_ref) ->
  let with_pcl f = with_ref pcl_ref f in

  let kvop_map_ops = Ins_del_op_type.default_kvop_map_ops () in
  let empty_map = kvop_map_ops.map_empty in
  let dcl_state = Dcl_types.{
      start_block=ptr0;
      current_block=ptr0;
      block_list_length=1;
      map_past=empty_map;
      map_current=empty_map
    } 
  in
  mk_ref dcl_state s |> fun (s,dcl_ref) ->
  let with_dcl f = with_ref dcl_ref f in

  let alloc () = with_free (fun ~state:free ~set_state -> 
      let free' = config.next_free_ptr free in
      set_state free' >>= fun () ->
      return free)
  in
  let dcl_ops = 
    make_dcl_ops'
      ~monad_ops
      ~config
      ~write_node
      ~alloc
      ~with_pl:{with_state=with_pl}
      ~with_pcl:{with_state=with_pcl}
      ~with_dcl:{with_state=with_dcl}
  in
  s,dcl_ops

let _ = make_dcl_ops_with_fun_store


(* on file ---------------------------------------------------------- *)

(** Construct write_node on top of a blk_dev *)

open Blk_dev


let make_dcl_ops_on_file ~monad_ops ~config ~fn = 
  (* NOTE the following uses ptr=int *)
  let blk_dev_ops = 
    Blk_dev_on_file.make_blk_dev_on_file ~monad_ops ~blk_sz:config.blk_sz in
  let fd = Tjr_fs_shared.File_util.fd_from_file ~fn ~create:true ~init:true in
  let write_node = write_node ~config ~blk_dev_ops ~dev:fd in
  let store = Tjr_store.initial_store in
  let store,dcl_ops = make_dcl_ops_with_fun_store ~config ~write_node ~store in
  fd,store,dcl_ops

let _ = make_dcl_ops_on_file


(* example, int int ------------------------------------------------- *)

open Tjr_monad.Monad_ops
open Tjr_monad.State_passing

let int_int_config = 
  let writer = Bin_prot.Type_class.bin_writer_int in
  let k_writer = writer in
  let v_writer = writer in
  let ptr_writer = writer in
  let ptr0 = 0 in
  let next_free_ptr = fun p -> p+1 in
  {
    ptr_sz=9;
    blk_sz=4096;
    k_size=9;
    v_size=9;
    k_writer;
    v_writer;
    ptr_writer;
    ptr0;
    next_free_ptr
  }

let test_dcl_ops_on_file ~fn ~count = 
  let config = int_int_config in
  let monad_ops : Tjr_store.t state_passing monad_ops = monad_ops () in
  let fd,store,dcl_ops = make_dcl_ops_on_file ~monad_ops ~config ~fn in
  let s = ref store in
  List.iter
    (fun i -> 
       run ~init_state:(!s) (dcl_ops.add (Insert(i,2*i)))
       |> fun (_,s') -> s:=s')       
    (Tjr_list.from_to 1 count)
  
  

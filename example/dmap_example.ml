(** An example use of the detachable map, for an on-disk persistent
   cache, with key type int, value type int

Suppose the block size is B bytes. Then we need xxx bytes (1 for list
   elt, 1 for option tag, 9 for ptr; xxx=11) for the End_of_list
   marker.

- (B-xxx) bytes are available for data

When we write eg Insert(k,v), we potentially use 1+|k|+|v| bytes. We
   need to be very careful that this doesn't exceed the B-xxx
   limit. Particularly, if values can be largeish (eg 256 bytes) we
   need to take great care.  *)

open Tjr_profile.Util.Profiler

open Pcache_intf
open Pcache_intf.Blk_dev_ops
open Pcache_intf.Ins_del_op

open Pcache_store_passing

let blk_sz = 4096

type buf = Bin_prot.Common.buf
let create_buf = Bin_prot.Common.create_buf

type ptr = int


module Config = struct

  type ('k,'v,'ptr) config = {
    ptr_sz:int;
    blk_sz:int;
    k_size:int;
    v_size:int;
    k_writer: 'k Bin_prot.Type_class.writer;
    k_reader: 'k Bin_prot.Type_class.reader;
    v_writer: 'v Bin_prot.Type_class.writer;
    v_reader: 'v Bin_prot.Type_class.reader;
    ptr_writer: 'ptr Bin_prot.Type_class.writer;
    ptr_reader: 'ptr Bin_prot.Type_class.reader;
    ptr0:'ptr; (* initial block *)
    next_free_ptr:'ptr -> 'ptr;
  }

  (* xxx *)
  let end_of_list_sz ~config = 1+1+config.ptr_sz   
  (* End_of_list, Some/None, and int ptr; assumes binprot *)

  let max_data_length ~config = config.blk_sz - (end_of_list_sz ~config)

  let int_int_config = 
    let writer = Bin_prot.Type_class.bin_writer_int in
    let reader = Bin_prot.Type_class.bin_reader_int in
    let ptr0 = 0 in
    let next_free_ptr = fun p -> p+1 in
    {
      ptr_sz=9;
      blk_sz=4096;
      k_size=9;
      v_size=9;
      k_writer=writer;
      k_reader=reader;
      v_writer=writer;
      v_reader=reader;
      ptr_writer=writer;
      ptr_reader=reader;
      ptr0;
      next_free_ptr
    }

  let config = int_int_config

end
open Config



module Fstore = struct
  
  let _fstore = ref Tjr_store.initial_store

  let alloc_fstore_ref = 
    fun x -> 
    Tjr_store.mk_ref x !_fstore |> fun (store',r) ->
    _fstore:=store';
    r

  (* initial block number *)
  let ptr0 = 0 
  let free_ref = alloc_fstore_ref ptr0 
  let with_free = with_ref free_ref

end
open Fstore


module Pcl_elt = struct
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
  let elt_reader ~config = 
    bin_reader_elt config.k_reader config.v_reader config.ptr_reader
end
open Pcl_elt


module Pl_impl = struct
  open Simple_pl_and_pcl_implementations
  type pl_data = buf*int
  type pl_internal_state = (pl_data,ptr) Pl_impl.pl_state

  let pl_ref = alloc_fstore_ref Pl_impl.{data=(create_buf 0,0); current=ptr0; next=None}
  let with_pl = with_ref pl_ref
  let pl_state_ops = Pl_impl.pl_state_ops

  let blk_dev_ops = 
    Blk_dev_on_file.make_blk_dev_on_file ~monad_ops ~blk_sz 

  let write_node ~config ~blk_dev_ops ~dev (pl_state: pl_internal_state) =
    mark "ab";
    let buf,pos = pl_state.data in
    let eol = End_of_list pl_state.next in
    let _pos = (elt_writer ~config).write buf ~pos eol in
    (* FIXME could be more efficient if we wrote direct to disk without
       going via string *)
    mark "ac";
    let blk = Core.Bigstring.to_string buf in
    mark "bc";
    blk_dev_ops.write ~dev ~blk_id:pl_state.current ~blk >>= fun x ->
    mark "cd"; return x

  let _ = write_node

  (* assume the fd is stored in the fstore *)
  let fd_ref = alloc_fstore_ref (None:Unix.file_descr option)
  let with_fd = with_ref fd_ref

  let write_node pl_state = 
    with_fd.with_state (fun ~state:fd ~set_state:_ -> 
        let fd = fd |> dest_Some in
        write_node ~config ~blk_dev_ops ~dev:fd pl_state)
end


module Pcl_impl = struct
  (* open Simple_pl_and_pcl_implementations *)

  (* NOTE specialize to int,int *)
  (* type k = int *)
  (* type v = int *)
  
  type pcl_internal_state = {
    pcl_data:buf*int;  (* NOTE this is the same as pl_data *)
  }

  (* type e = (k,v)Pcache_intf.op *)
  
  let pcl_state_ops ~(config:('k,'v,'ptr)config) = 
    assert(config.k_size + config.v_size+1 <= max_data_length ~config);
    Pcache_intf.Pcl_types.{
      nil=(fun () -> {pcl_data=(create_buf config.blk_sz,0)});
      snoc=(fun pcl_state (e:('k,'v)op) -> 
          (* remember that we have to leave 10 bytes for the
             "End_of_list" marker *)        
          let buf,pos = pcl_state.pcl_data in
          let e' = match e with
            | Insert(k,v) -> Pcl_elt.Insert(k,v)
            | Delete k -> Pcl_elt.Delete k
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
            `Ok {pcl_data=(buf,pos')}
          | false -> `Error_too_large       
        ); 
      pl_data=(fun pcl_state -> pcl_state.pcl_data)
    }

  let pcl_state_ops = pcl_state_ops ~config 

  let pcl_ref = alloc_fstore_ref {pcl_data=(create_buf 0,0)}
  let with_pcl = with_ref pcl_ref
end


module With_dmap = struct
  (* NOTE specialize to int,int *)
  (* let kvop_map_ops : (int,int,_)Tjr_map.map_ops = Op_aux.default_kvop_map_ops () *)

  open Pcache_intf.Dcl_types

  let dmap_ref = 
    let kvop_map_ops = Op_aux.default_kvop_map_ops () in
    let empty_map = kvop_map_ops.empty in
    alloc_fstore_ref 
      { start_block=ptr0;
        current_block=ptr0;
        block_list_length=1;
        abs_past=empty_map;
        abs_current=empty_map }

  let with_dmap : ((ptr,int,int)Dmap_types.dmap_state,fstore_passing)with_state = with_ref dmap_ref
  let _ = dmap_ref
end

module S = struct

  type k = int
  type v = int
  type nonrec ptr = ptr
  type t = fstore_passing
  let monad_ops = Pcache_store_passing.monad_ops

  let alloc () = with_free.with_state (fun ~state:free ~set_state -> 
      let free' = free+1 in
      set_state free' >>= fun () ->
      return free)

  include Pl_impl

  include Pcl_impl

  type e = (k,v) Pcache_intf.op

  include With_dmap
end


module Pcache = Tjr_pcache.Generic_make_functor.Make(S)


(*
(* config params ---------------------------------------------------- *)

type ('k,'v,'ptr) config = {
  ptr_sz:int;
  blk_sz:int;
  k_size:int;
  v_size:int;
  k_writer: 'k Bin_prot.Type_class.writer;
  k_reader: 'k Bin_prot.Type_class.reader;
  v_writer: 'v Bin_prot.Type_class.writer;
  v_reader: 'v Bin_prot.Type_class.reader;
  ptr_writer: 'ptr Bin_prot.Type_class.writer;
  ptr_reader: 'ptr Bin_prot.Type_class.reader;
  ptr0:'ptr; (* initial block *)
  next_free_ptr:'ptr -> 'ptr;
}

(* xxx *)
let end_of_list_sz ~config = 1+1+config.ptr_sz   
(* End_of_list, Some/None, and int ptr; assumes binprot *)

let max_data_length ~config = config.blk_sz - (end_of_list_sz ~config)






(* pl instance ------------------------------------------------------ *)

(* at the pl level:
data : 'a = buf * pos
'i = buf,pos,next

at the pcl level, again we can take 

'a = buf * pos
'e = op
'i = op list as buf * pos

*)


(* type 'ptr pl_state = {
 *   data: buf * int;
 *   current: 'ptr;
 *   next: 'ptr option
 * } *)

open Blk_dev_on_file
open Simple_pl_and_pcl_implementations

open Pl_impl  (* record aux fns *)
type 'ptr pl_state = (buf*int,'ptr) Pl_impl.pl_state
let pl_state_ops = Pl_impl.pl_state_ops

let write_node ~config ~blk_dev_ops ~dev (pl_state: 'ptr pl_state) =
  mark "ab";
  let buf,pos = pl_state.data in
  let eol = End_of_list pl_state.next in
  let _pos = (elt_writer ~config).write buf ~pos eol in
  (* FIXME could be more efficient if we wrote direct to disk without
     going via string *)
  mark "ac";
  let blk = Core.Bigstring.to_string buf in
  mark "bc";
  blk_dev_ops.write ~dev ~blk_id:pl_state.current ~blk >>= fun x ->
  mark "cd"; return x

let _ = write_node



let read_node ~monad_ops ~config ~blk_dev_ops ~dev ~blk_id =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  blk_dev_ops.read ~dev ~blk_id >>= fun (blk:string) ->
  let buf = Bin_prot.Common.create_buf config.blk_sz in
  Bin_prot.Common.unsafe_blit_string_buf ~src_pos:0 blk ~dst_pos:0 buf ~len:config.blk_sz;
  (* now we convert buf to a seq of elts *)
  let reader = elt_reader ~config in
  let open Pcache_intf.Ins_del_op in
  let rec read pos_ref elts_so_far (* reversed *) =
    reader.read buf ~pos_ref |> fun (elt:('k,'v,'ptr)elt) -> 
    match elt with
    (* read till we reach end_of_list *)
    | End_of_list ptr_opt -> (List.rev elts_so_far,ptr_opt)
    | Insert(k,v) -> 
      let elt = Insert(k,v) in
      read pos_ref (elt::elts_so_far)
    | Delete k -> 
      let elt = Delete k in
      read pos_ref (elt::elts_so_far)
  in
  return (read (ref 0) [])
    

(* FIXME can generalize over string? *)
let _ :
monad_ops:'a monad_ops ->
config:('k, 'v, 'ptr) config ->
blk_dev_ops:('b, string, 'c, 'a) blk_dev_ops ->
dev:'c -> blk_id:'b -> (('k, 'v) op list * 'ptr option, 'a) m
= read_node

(* let pl_state_ops = Pl_types.{
 *   set_data=(fun data pl_state -> {pl_state with data});
 *   set_next=(fun ptr pl_state -> {pl_state with next=(Some ptr)});
 *   new_node=(fun current data pl_state -> { data; current; next=None })
 * }   *)
  


(* pcl -------------------------------------------------------------- *)

type pcl_state = {
  data:buf*int;
}

let pcl_state_ops ~(config:('k,'v,'ptr)config) = 
  assert(config.k_size + config.v_size+1 <= max_data_length ~config);
  Pcache_intf.Pcl_types.{
    nil=(fun () -> {data=(create_buf config.blk_sz,0)});
    snoc=(fun pcl_state (e:('k,'v)op) -> 
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


(* dcl/dmap --------------------------------------------------------- *)

(* nothing to do *)


(* everything ------------------------------------------------------- *)



(* open Tjr_monad.State_passing *)
(* open Ins_del_op_type *)

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


let make_dmap_dcl_ops' 
    ~(monad_ops:'t monad_ops) 
    ~(config:('k,'v,'ptr)config) 
    ~(write_node:('ptr pl_state -> (unit,'t)m))
    ~alloc 
    ~with_pl 
    ~with_pcl 
    ~with_dmap
  =
  let pcl_ops = 
    make_pcl_ops' ~monad_ops ~config ~write_node ~alloc ~with_pl ~with_pcl 
  in
  Detachable_map.make_dmap_dcl_ops
    ~monad_ops
    ~pcl_ops
    ~with_dmap

let _ :
monad_ops:'t monad_ops ->
config:('k, 'v, 'ptr) config ->
write_node:('ptr pl_state -> (unit, 't) m) ->
alloc:(unit -> ('ptr, 't) m) ->
with_pl:((buf * int, 'ptr) Pl_impl.pl_state, 't) with_state ->
with_pcl:(pcl_state, 't) with_state ->
with_dmap:(('ptr, 'k, 'v) Pcache_intf.Dmap_types.dmap_state, 't) with_state ->
('ptr, 'k, 'v, 't) Pcache_intf.Dmap_types.dmap_dcl_ops
= make_dmap_dcl_ops'

(* let monad_ops : Tjr_store.t state_passing monad_ops = monad_ops () *)


open Pcache_store_passing


(* with fun store --------------------------------------------------- *)

(** Construct dcl ops on top of a functional store. *)
let make_dmap_ops_with_fun_store 
    ~(config:('k,'v,'ptr)config) 
    ~write_node 
    ~store 
  =
  let open Tjr_store in
  (* get some refs *)
  let ptr0 = config.ptr0 in
  mk_ref (config.next_free_ptr ptr0) store |> fun (s,free_ref) ->
  let with_free = with_ref free_ref in

  let data = (Bin_prot.Common.create_buf config.blk_sz,0) in
  let pl_state = { 
    data;
    current=ptr0;
    next=None }
  in
  mk_ref pl_state s |> fun (s,pl_ref) ->
  let with_pl = with_ref pl_ref in

  let pcl_state = { data } in
  mk_ref pcl_state s |> fun (s,pcl_ref) ->
  let with_pcl = with_ref pcl_ref in

  let kvop_map_ops = Op_aux.default_kvop_map_ops () in
  let empty_map = kvop_map_ops.empty in
  let dmap_state = Dcl_types.{
      start_block=ptr0;
      current_block=ptr0;
      block_list_length=1;
      abs_past=empty_map;
      abs_current=empty_map
    } 
  in
  mk_ref dmap_state s |> fun (s,dmap_ref) ->
  let with_dmap = with_ref dmap_ref in

  let alloc () = with_free.with_state (fun ~state:free ~set_state -> 
      let free' = config.next_free_ptr free in
      set_state free' >>= fun () ->
      return free)
  in
  let dmap_ops = 
    make_dmap_dcl_ops'
      ~monad_ops
      ~config
      ~write_node
      ~alloc
      ~with_pl
      ~with_pcl
      ~with_dmap
  in
  s,dmap_ops

let _ = make_dmap_ops_with_fun_store


(* on file ---------------------------------------------------------- *)

(** Construct write_node on top of a blk_dev *)

(* open Blk_dev_on_file *)

let make_dmap_ops_on_file ~monad_ops ~config ~fn = 
  (* NOTE the following uses ptr=int *)
  let blk_dev_ops = 
    Blk_dev_on_file.make_blk_dev_on_file ~monad_ops ~blk_sz:config.blk_sz in
  let fd = Tjr_file.fd_from_file ~fn ~create:true ~init:true in
  let write_node = write_node ~config ~blk_dev_ops ~dev:fd in
  let store = Tjr_store.initial_store in
  let store,dmap_ops = 
    make_dmap_ops_with_fun_store ~config ~write_node ~store in
  fd,store,dmap_ops

let _ = make_dmap_ops_on_file


(* example, int int ------------------------------------------------- *)

(* open Tjr_monad.State_passing *)

let int_int_config = 
  let writer = Bin_prot.Type_class.bin_writer_int in
  let reader = Bin_prot.Type_class.bin_reader_int in
  let ptr0 = 0 in
  let next_free_ptr = fun p -> p+1 in
  {
    ptr_sz=9;
    blk_sz=4096;
    k_size=9;
    v_size=9;
    k_writer=writer;
    k_reader=reader;
    v_writer=writer;
    v_reader=reader;
    ptr_writer=writer;
    ptr_reader=reader;
    ptr0;
    next_free_ptr
  }

let config = int_int_config

(* let monad_ops = Pcache_store_passing.monad_ops *)

let blk_dev_on_file = 
  Blk_dev_on_file.make_blk_dev_on_file ~monad_ops ~blk_sz:config.blk_sz

let test_dmap_ops_on_file ~fn ~count = 
  let open Util in
  let profiler = Tjr_profile.make_string_profiler ~now in
  Util.profile_function "test_dmap_ops_on_file" @@ fun () -> 
  let fd,store,dmap_ops = make_dmap_ops_on_file ~monad_ops ~config ~fn in
  let s = ref store in
  List_.from_to 1 count |> List.iter
    (fun i -> 
       profiler.mark "za";
       Pcache_store_passing.run ~init_state:(!s) (dmap_ops.add (Insert(i,2*i)))
       |> fun (_,s') -> profiler.mark "zb"; s:=s');
  profiler.print_summary();
  Unix.close fd



(* read data from file ------------------------------------------------ *)
let _ = read_node

let read_node ~dev ~blk_id = 
  read_node ~monad_ops ~config ~blk_dev_ops:blk_dev_on_file ~dev ~blk_id

let read_back ~fn =
  (* let monad_ops : Tjr_store.t state_passing monad_ops = monad_ops () in *)
  let fd = Tjr_file.fd_from_file ~fn ~create:false ~init:false in
  let read_node ptr _blks = read_node ~dev:fd ~blk_id:ptr in
  let read_node ptr blks =
    read_node ptr blks 
    |> Pcache_store_passing.run ~init_state:Tjr_store.initial_store
    |> fun (ess,_) -> ess
  in
  let _ = read_node in
  let ess = 
    Persistent_chunked_list.pcl_to_elt_list_list
      ~read_node ~ptr:config.ptr0 ~blks:() 
  in
  Unix.close fd;
  ess

let _ = read_back
*)

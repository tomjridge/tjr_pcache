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



module Write_profiler = Make_profiler()
open Write_profiler

open Pcache_intf
open Pcache_intf.Blk_dev_ops
open Pcache_intf.Ins_del_op

open Pcache_store_passing

type buf = Bin_prot.Common.buf
let create_buf = Bin_prot.Common.create_buf

module Pcl_internal_state = struct
  type pcl_internal_state = {
    pcl_data:buf*int;  (* NOTE this is the same as pl_data *)
  }
end
(* open Pcl_internal_state *)


(* blk ptr *)
type ptr = int


(*
module Test_config = struct

  module C = struct
   
    type config = {
      block_writes: bool
      (** we may want to disable block writes, in order to assess the time spent writing to disk *)
    }

    let default_config = Some { block_writes=true }
  end

  module D = Tjr_config.Make(C)
   

end
*)


(** Various other bits of configuration (typically how to marshal k,v etc) *)
module Config = struct

  let ptr0 = 0 

  let blk_sz = 4096

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


(** Track the various bits of state by using a functional store:

- free (free block ptr)
- fd (block device via file descriptor)
- pl  
- pcl  
- dmap

*)
module Fstore = struct
  
  (* NOTE we allow_reset, but only for fd_ref (and free ref?) *)
  module R = Tjr_store.Make_imperative_fstore(struct let allow_reset=true end)
  (* initial block number *)
  let free_ref = R.ref (config.next_free_ptr ptr0)

  let fd_ref = R.ref (None:Unix.file_descr option)
  let pl_ref = R.mk_uref ~name:"pl_ref" 
  let pcl_ref = R.mk_uref ~name:"pcl_ref"
  let dmap_ref = R.mk_uref ~name:"dmap_ref"

  let with_free = with_ref free_ref
  let with_fd = with_ref fd_ref 
  let with_pl = with_ref pl_ref
  let with_pcl = with_ref pcl_ref
  let with_dmap = with_ref dmap_ref

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
  (* at the pl level:
data : 'a = buf * pos
'i = buf,pos,next

at the pcl level, again we can take 

'a = buf * pos
'e = op
'i = op list as buf * pos

  *)
  open Simple_pl_and_pcl_implementations
  type pl_data = buf*int
  type pl_internal_state = (pl_data,ptr) Pl_impl.pl_state
      
  
  let _ = 
    R.(pl_ref := Pl_impl.{data=(create_buf blk_sz,0); current=ptr0; next=None})

  let pl_state_ops = Pl_impl.pl_state_ops

  let blk_dev_ops = 
    Blk_dev_on_file.make_blk_dev_on_file ~monad_ops ~blk_sz 

  let write_count = ref 0

  let _ = Pervasives.at_exit (fun () -> 
      Printf.printf "Blk write count: %d\n" !write_count)

  let write_node ~config ~blk_dev_ops ~dev (pl_state: pl_internal_state) =
    mark "start";
    let buf,pos = pl_state.data in
    let eol = End_of_list pl_state.next in
    let _pos = (elt_writer ~config).write buf ~pos eol in
    (* FIXME could be more efficient if we wrote direct to disk without
       going via string *)
    mark "buf2string";
    let blk = Core.Bigstring.to_string buf in
    mark "buf2string'";
    blk_dev_ops.write ~dev ~blk_id:pl_state.current ~blk >>= fun x ->
    incr write_count;
    mark "end"; return x

  let _ = write_node

  (* assume the fd is stored in the fstore *)
  let write_node pl_state = 
    with_fd.with_state (fun ~state:fd ~set_state:_ -> 
        let fd = fd |> dest_Some in
        write_node ~config ~blk_dev_ops ~dev:fd pl_state)

  let with_pl = with_pl
end


module Internal_read_node = struct

  let read_node ~monad_ops ~config ~blk_dev_ops ~dev ~blk_id =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    (* FIXME for performance, try to avoid string conversions *)
    blk_dev_ops.read ~dev ~blk_id >>= fun (blk:string) ->
    let buf = create_buf config.blk_sz in
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

  let read_node ~dev ~blk_id = 
    read_node ~monad_ops ~config ~blk_dev_ops:Pl_impl.blk_dev_ops ~dev ~blk_id

  let read_back ~fn =
    (* let monad_ops : Tjr_store.t state_passing monad_ops = monad_ops () in *)
    let fd = Tjr_file.fd_from_file ~fn ~create:false ~init:false in
    let read_node ptr _blks = read_node ~dev:fd ~blk_id:ptr in
    let read_node ptr blks =
      read_node ptr blks 
      |> Pcache_store_passing.run ~init_state:(Tjr_store.empty_fstore ())
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
end


module Pcl_impl = struct

  include Pcl_internal_state
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

  let _ = 
    R.(pcl_ref := {pcl_data=(create_buf blk_sz,0)})

  let with_pcl = with_pcl
end


module With_dmap = struct
  open Pcache_intf.Dcl_types

  let _ =  
    (* FIXME this should be elsewhere *)
    let kvop_map_ops = Op_aux.default_kvop_map_ops () in
    let empty_map = kvop_map_ops.empty in
    R.(dmap_ref := { start_block=ptr0;
                     current_block=ptr0;
                     block_list_length=1;
                     abs_past=empty_map;
                     abs_current=empty_map })

  let with_dmap = with_dmap
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


module Pcache : sig 
  val make_dmap_on_file :
    fn:string -> Unix.file_descr * fstore * (int, int, ptr, fstore_passing) Dmap_types.dmap_ops
  val pl_sync : unit -> (unit, fstore_passing) m
end = struct
  module Internal2 = struct
    include Tjr_pcache.Generic_make_functor.Make(S)

    (** NOTE this needs to have the fd set before use *)
    let initial_store = R.fstore

    let set_fd_in_initial_store ~fd = 
      !initial_store |> Tjr_store.set fd_ref (Some fd) |> fun s ->
      initial_store:=s

    let make_dmap_on_file ~fn =
      let fd = Tjr_file.fd_from_file ~fn ~create:true ~init:true in
      set_fd_in_initial_store ~fd;
      fd,!initial_store,dmap_ops
  end

  let make_dmap_on_file = Internal2.make_dmap_on_file

  let pl_sync = Internal2.pl_sync
end





module Test = struct

  module Profiler1 = Make_profiler()
  open Profiler1

  let detach_interval = 1000

  let test_dmap_ops_on_file ~fn ~count = 
    mark "start1";
    let fd,store,dmap_ops = Pcache.make_dmap_on_file ~fn in
    let s = ref store in
    mark "start2";
    1 |> List_.iter_break 
           (fun i -> 
              match i > count with
              | true -> `Break ()
              | false -> 
                match i mod detach_interval = 0 with
                | false -> (
                    mark "test_ins";
                    Pcache_store_passing.run ~init_state:(!s) (dmap_ops.insert i (2*i))
                    |> fun (_,s') -> mark "test_ins'"; s:=s';
                    `Continue (i+1))

                | true -> (
                    mark "detach";
                    Pcache_store_passing.run ~init_state:(!s) (dmap_ops.detach ())
                    |> fun (_,s') -> mark "detach'"; s:=s';
                    `Continue (i+1)));
    mark "end'";
    Tjr_profile.measure_execution_time_and_print "final_sync" (fun () -> 
      Pcache_store_passing.run ~init_state:!s (Pcache.pl_sync ()) |> fun ((),_s') -> ());
    Unix.close fd;
    if true || profiling_enabled then (  (* always print for the moment *)
      Printf.printf "\nTop-level profiler\n";Profiler1.print_summary();
      Printf.printf "\nWrite profiler\n";Write_profiler.print_summary();
      Printf.printf "\nPl profiler\n";Persistent_list.Pl_profiler.print_summary();
      Printf.printf "\nPcl profiler\n";Persistent_chunked_list.Pcl_profiler.print_summary();
      Printf.printf "\nPcl micro profiler\n";Persistent_chunked_list.M.print_summary();
      Printf.printf "\nDcl profiler\n";Detachable_chunked_list.Dcl_profiler.print_summary();
      Printf.printf "\nDcl micro profiler\n";Detachable_chunked_list.Micro.print_summary();
      Printf.printf "\nDmap profiler\n";Detachable_map.Dmap_profiler.print_summary())
    else ()
end



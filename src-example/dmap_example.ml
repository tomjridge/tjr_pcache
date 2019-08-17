(** An example use of the detachable map, for an on-disk persistent
   cache, with key type int, value type int

    Suppose the block size is B bytes. Then we need xxx bytes (1 for
    list elt, 1 for option tag, 9 for ptr; xxx=11) for the End_of_list
    marker.

    - (B-xxx) bytes are available for data

    When we write eg Insert(k,v), we potentially use 1+|k|+|v| bytes. We
    need to be very careful that this doesn't exceed the B-xxx
    limit. Particularly, if values can be largeish (eg 256 bytes) we
    need to take great care.

*)

open Pcache_intf
open Pcache_intf.Ins_del_op


(** {2 Blocks etc} *)

module Blk_id = Blk_id_as_int
type blk_id = Blk_id.blk_id
let blk_sz = Blk_sz.blk_sz_4096
let blk_sz_as_int = blk_sz |> Blk_sz.to_int

(* open Pcache_store_passing *)

(** {2 Buffers} *)

module Buf = struct
  type buf = Bin_prot.Common.buf
  let create_buf = Bin_prot.Common.create_buf
end
include Buf


(** {2 Marshalling config} *)

open Pcache_example_intf

(** Various other bits of configuration (typically how to marshal k,v etc) *)
module Marshalling_config = struct

  (* xxx *)
  let end_of_list_sz ~config = 1+1+config.ptr_sz
  (* End_of_list, Some/None, and int ptr; assumes binprot *)

  let max_data_length ~config = config.blk_sz - (end_of_list_sz ~config)

  let int_int_config =
    let writer = Bin_prot.Type_class.bin_writer_int in
    let reader = Bin_prot.Type_class.bin_reader_int in
    let next_free_ptr = fun p -> p|>Blk_id.to_int |> fun p -> p+1 |> Blk_id.of_int in
    {
      ptr_sz=9;
      blk_sz=4096;
      k_size=9;
      v_size=9;
      k_writer=writer;
      k_reader=reader;
      v_writer=writer;
      v_reader=reader;
      ptr_writer=Blk_id.bin_writer_blk_id;
      ptr_reader=Blk_id.bin_reader_blk_id;
      (* ptr0; *)
      next_free_ptr
    }

  (* let config = int_int_config *)

end
open Marshalling_config



type pcl_internal_state = {
  pcl_data:buf*int;  (* NOTE this is the same as pl_data *)
}


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

  let _ = elt_writer

  let elt_reader ~config =
    bin_reader_elt config.k_reader config.v_reader config.ptr_reader
end
open Pcl_elt


(** Read a node back from disk; this returns an (kvop list * ptr option) *)
module Pcl_read_node = struct

    let read_node ~monad_ops ~config ~blk_dev_ops ~blk_id =
      let ( >>= ) = monad_ops.bind in
      let return = monad_ops.return in
      (* FIXME for performance, try to avoid string conversions *)
      blk_dev_ops.read ~blk_id >>= fun (blk:string) ->
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

    let _ 
: monad_ops:'a monad_ops ->
config:('k, 'v, 'ptr) marshalling_config ->
blk_dev_ops:('b, string, 'a) blk_dev_ops ->
blk_id:'b -> (('k, 'v) kvop list * 'ptr option, 'a) m
= read_node
end

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
  type pl_internal_state = (pl_data,blk_id) Pl_impl.pl_state

  let initial_pl_state ~data ~current ~next = Pl_impl.{data;current;next}

  let pl_state_ops = Pl_impl.pl_state_ops
end


module Pcl_impl = struct

  let pcl_state_ops ~(config:('k,'v,'ptr)marshalling_config) =
    assert(config.k_size + config.v_size+1 <= max_data_length ~config);
    Pcache_intf.Pcl_types.{
      nil=(fun () -> {pcl_data=(create_buf config.blk_sz,0)});
      snoc=(fun pcl_state (e:('k,'v)kvop) ->
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

  let initial_pcl_internal_state ~pcl_data = {pcl_data}
end






(** {2 Construct the layers} *)

(** Use the Pcache_intf.pcache_layers type *)
module Make_layers = struct


  (** independent of monad *)

  let blk_ops = Common_blk_ops.String_.make ~blk_sz

  type buf = Bin_prot.Common.buf
  type blk_id = Blk_id_as_int.blk_id

  module S = Simple_pl_and_pcl_implementations

  type pl_data = buf*int
  type pl_internal_state = (pl_data,blk_id) S.Pl_impl.pl_state

  let pl_state_ops = S.Pl_impl.pl_state_ops

  (* type ('ptr, 'k, 'v) dmap_state = ('ptr, ('k, 'v) op_map) Dcl_types.dcl_state *)

  (* FIXME add a {dmap_state} wrapper for dmap_state type *)
  let initial_dmap_state ~(dcl_state:('r,('k,'v)kvop_map) Dcl_types.dcl_state) = dcl_state

  let read_back ~monad_ops ~blk_dev_ops ~config ~blk_id = 
    let read_node blk_id = Pcl_read_node.read_node ~monad_ops ~config ~blk_dev_ops ~blk_id in
    let ess =
      Persistent_chunked_list.pcl_to_elt_list_list
        ~monad_ops
        ~read_node 
        ~ptr:blk_id
    in
    ess

  let _ 
: monad_ops:'a monad_ops ->
blk_dev_ops:('b, string, 'a) blk_dev_ops ->
config:('c, 'd, 'b) marshalling_config ->
blk_id:'b -> (('c, 'd) kvop list list, 'a) m
= read_back

  let blk_write_count = ref 0
  let _ = Pervasives.at_exit (fun () ->
      Printf.printf "Blk write count (%s): %d\n" __FILE__ !blk_write_count)

  let write_node ~monad_ops ~(elt_writer:'a Bin_prot.Type_class.writer) ~blk_dev_ops = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    fun ~(pl_state: Pl_impl.pl_internal_state) ->
      (* mark "start"; *)
      let buf,pos = pl_state.data in
      let eol = End_of_list pl_state.next in
      let _pos = elt_writer.write buf ~pos eol in
      (* FIXME could be more efficient if we wrote direct to disk without
         going via string *)
      (* mark "buf2string"; *)
      let blk = Core.Bigstring.to_string buf in
      (* mark "buf2string'"; *)
      blk_dev_ops.write ~blk_id:pl_state.current ~blk >>= fun x ->
      incr blk_write_count;
      (* mark "end";  *)
      return x

  let make_layers (type k v t) ~monad_ops ~config ~blk_dev_ops = 
    let module A = struct
      let elt_writer = Pcl_elt.elt_writer ~config
      let elt_reader = Pcl_elt.elt_reader ~config

      let read_back ~blk_dev_ops ~blk_id = read_back ~monad_ops ~blk_dev_ops ~config ~blk_id

      let write_node ~blk_dev_ops = write_node ~monad_ops ~elt_writer ~blk_dev_ops
      let _ = write_node

      let pcl_state_ops = Pcl_impl.pcl_state_ops ~config
      let _ = pcl_state_ops

      let alloc = ref None
      let with_pl = ref None
      let with_pcl = ref None
      let with_dmap = ref None

      let refs_initialized () = 
        match () with
        | _ when !alloc = None -> 
          Printf.sprintf "dmap_ops not initialized, at %s\n" __LOC__
          |> failwith
        | _ when !with_pl = None -> 
          Printf.sprintf "with_pl not initialized, at %s\n" __LOC__
          |> failwith
        | _ when !with_pcl = None -> 
          Printf.sprintf "with_pcl not initialized, at %s\n" __LOC__
          |> failwith
        | _ when !with_dmap = None -> 
          Printf.sprintf "with_map not initialized, at %s\n" __LOC__
          |> failwith
        | _ -> true


      let dmap_ops ~write_node = 
        assert(refs_initialized ());
        (* FIXME might also have a version where with_pl etc are specialized to stdlib.refs *)
        let alloc,with_pl,with_pcl,with_dmap = 
          (!alloc)|>dest_Some,(!with_pl)|>dest_Some,(!with_pcl)|>dest_Some,(!with_dmap)|>dest_Some
        in
        let module Internal = struct
          type nonrec k = k
          type nonrec v = v
          type nonrec ptr = blk_id
          type nonrec t = t
          let monad_ops = monad_ops

          type nonrec pl_data = pl_data
          type nonrec pl_internal_state = pl_internal_state
          let pl_state_ops = pl_state_ops

          type nonrec pcl_internal_state = pcl_internal_state
          type e = (k,v) kvop
          let pcl_state_ops = Pcl_impl.pcl_state_ops ~config
        end
        in
        let module G = Tjr_pcache.Generic_make_functor.Make(Internal) in
        let dmap_ops  = (G.make_dmap_ops ~alloc ~with_pl ~with_pcl ~with_dmap ~write_node).dmap_ops in
        dmap_ops        

    end
    in
    A.{
      monad_ops;
      config;
      elt_writer;
      elt_reader;
      blk_write_count;
      blk_ops;
      blk_dev_ops;
      write_node;
      read_back;
      pl_state_ops;
      pcl_state_ops;
      (* initial_states; *)
      alloc;
      with_pl;
      with_pcl;
      with_dmap;
      dmap_ops
    }

  let _
: monad_ops:'a monad_ops ->
config:('b, 'c, blk_id) marshalling_config ->
blk_dev_ops:('d -> (blk_id, string, 'a) blk_dev_ops) ->
('b, 'c, blk_id, 'a, string, Pl_impl.pl_data,
 (Pl_impl.pl_data * blk_id * blk_id option) * pcl_internal_state *
 (blk_id, ('b, 'c) kvop_map) Dcl_types.dcl_state,
 Pl_impl.pl_internal_state, pcl_internal_state, ('b, 'c, blk_id) elt, 'd,
 ('b, 'c) kvop)
pcache_layers
= make_layers


  module Int_int = struct
    type k = int
    type v = int
    let config = Marshalling_config.int_int_config
  end


  module With_lwt = struct
    
    let monad_ops = lwt_monad_ops

    let blk_dev_ops fd =
        Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd    

    let make_int_int_layers () = 
      let open Int_int in
      make_layers ~monad_ops ~config ~blk_dev_ops

    let _ 
: unit ->
(int, int, blk_id, lwt, string, Pl_impl.pl_data,
  (Pl_impl.pl_data * blk_id * blk_id option) * pcl_internal_state *
 (blk_id, (int, int) kvop_map) Dcl_types.dcl_state,
 Pl_impl.pl_internal_state, pcl_internal_state, (int, int, blk_id) elt,
 Lwt_unix.file_descr, (int, int) kvop)
pcache_layers
= make_int_int_layers

  end
end


(** Auxiliary module to help with initialization *)
module Initial_states = struct

  include Pl_impl
  type ('a,'r) pl_state = ('a,'r) Simple_pl_and_pcl_implementations.Pl_impl.pl_state
  include Buf
  include Dcl_types
  type ('k,'v) op_map = ('k,'v) kvop_map

  let initial_pl_state : data:Pl_impl.pl_data ->
current:'r ->
next:'r option -> ('a, 'r) pl_state
    = Pl_impl.initial_pl_state

  let initial_pcl_state ~(buf:buf) ~int = Pcl_impl.initial_pcl_internal_state ~pcl_data:(buf,int)
  let _ = initial_pcl_state


  let initial_dcl_state ~(start_block:'r) ~current_block ~block_list_length 
      ~(past:(('k, 'v) kvop) list list)
      ~(current:('k, 'v) kvop list)
    = 
    (* FIXME rename Kv_op to Kvop ? *)
    let kvop_map_ops = Kv_op.default_kvop_map_ops() in
    let kvop_to_key = Kv_op.op2k (* FIXME remove *) in
    let list_to_map kvops = (* FIXME replace with Kv_op.list_to_map *)
      kvops 
      |> List.map (fun kvop -> (kvop_to_key kvop,kvop))
      |> kvop_map_ops.of_bindings
    in
    let abs_past = 
      (kvop_map_ops.empty,past) |> iter_k (fun ~k -> fun (acc,past) ->
          match past with
          | [] -> acc
          | x::xs ->
            list_to_map x |> fun m ->
            Tjr_map.map_merge ~map_ops:kvop_map_ops ~old:acc ~new_:m 
            |> fun acc -> 
            k (acc,xs))
    in     
    let abs_current = list_to_map current in
    { start_block; current_block; block_list_length; 
      abs_past;
      abs_current }

  let _ 
: start_block:'r ->
current_block:'r ->
block_list_length:int ->
past:('k, 'v) kvop list list ->
current:('k, 'v) kvop list ->
('r, ('k, ('k, 'v) kvop, 'a) Tjr_map.map) dcl_state
= initial_dcl_state

  let initial_dmap_state
    : dcl_state:('r, ('k, 'v) op_map) dcl_state ->
      ('r, ('k, 'v) op_map)dcl_state
    = Make_layers.initial_dmap_state

end

(* module M = Initial_states *)


(* FIXME
  module Test = struct

    let detach_interval = 100


    let test_dmap_ops_on_file ~fn ~count =
      (* mark "start1"; *)
      let config = Config.int_int_config in
      let x = Common_dmap_on_file_instances.int_int ~fn in
      let (fd,store) = x.initial_state in
      let Generic_make_functor.{dmap_ops;pl_sync=_} = x.ops in
      let s = ref store in
      let run m =
        Pcache_store_passing.run ~init_state:(!s) m |> fun (r,s') ->
        s:=s';
        r
      in
      (* mark "start2"; *)

      (* loop count times, inserting kv pair *)
      begin
        1 |> iter_break
          (fun i ->
             match i > count with
             | true -> Break ()
             | false ->
               let _maybe_detach =
                 (* FIXME enable detach NOTE detach improves performance as expected *)
                 match false (* i mod detach_interval = 0 *) with
                 | false -> ()
                 | true -> (
                     (* mark "detach"; *)
                     ignore(run (dmap_ops.detach ()));
                     ())
               in
               (* mark "test_ins"; *)
               run (dmap_ops.insert i (2*i))
               |> fun _ -> Cont (i+1))
      end;
      run (dmap_ops.dmap_write ()); (* FIXME needed? isn't this the same as sync? *)
      Tjr_profile.measure_execution_time_and_print "final_sync" (fun () ->
          run (dmap_ops.dmap_sync ())); (* FIXME if dmap_ops has sync, why do we need pl_sync? *)
      Unix.close fd;
      Tjr_profile.measure_execution_time_and_print "read_back" (fun () ->
          let run = Pcache_store_passing.run in
          Unix_blk_layer.Internal_read_node.read_back ~monad_ops ~config ~fn ~ptr0 ~run |> fun ess ->
          Printf.printf "read back %d ops\n%!" (List.length (List.concat ess)));
      ()

  end
end
*)

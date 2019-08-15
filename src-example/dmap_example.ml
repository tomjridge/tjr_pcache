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
module Blk_id = Blk_id_as_int
open Blk_id
let blk_sz = Blk_sz.blk_sz_4096
let blk_sz_as_int = blk_sz |> Blk_sz.to_int

(* open Pcache_store_passing *)

type buf = Bin_prot.Common.buf
let create_buf = Bin_prot.Common.create_buf

module Pcl_internal_state = struct
  type pcl_internal_state = {
    pcl_data:buf*int;  (* NOTE this is the same as pl_data *)
  }
end

(** Various other bits of configuration (typically how to marshal k,v etc) *)
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
    (* ptr0:'ptr; (\* initial block *\) *)
    next_free_ptr:'ptr -> 'ptr;
  }

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
open Config



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




module Unix_blk_layer = struct
  let blk_ops = Common_blk_ops.String_.make ~blk_sz

  let blk_dev_ops ~monad_ops ~fd =
    Blk_dev_on_fd.make_with_unix ~monad_ops ~blk_ops ~fd

  let _ = blk_dev_ops

  module Internal_read_node = struct

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


    (* FIXME can generalize over string? *)
    let _ :
      monad_ops:'a monad_ops ->
      config:('k, 'v, 'ptr) config ->
      blk_dev_ops:('b, string, 'a) blk_dev_ops ->
      blk_id:'b -> (('k, 'v) Ins_del_op.op list * 'ptr option, 'a) m
      = read_node

    (* let read_node ~blk_id = read_node ~monad_ops ~config ~blk_dev_ops ~blk_id *)

    (* FIXME we probably want this to be imperative, just for ease of use *)
    let read_back ~monad_ops ~config ~fn ~ptr0 ~run =
      let fd = Tjr_file.fd_from_file ~fn ~create:false ~init:false in
      let blk_dev_ops = blk_dev_ops ~monad_ops ~fd in
      let read_node ptr _blks = read_node ~monad_ops ~config ~blk_dev_ops ~blk_id:ptr in
      let read_node ptr blks =
        read_node ptr blks
        |> run ~init_state:(Tjr_store.empty_fstore ())
        |> fun (ess,_) -> ess
      in
      let _ = read_node in
      let ess =
        Persistent_chunked_list.pcl_to_elt_list_list
          ~read_node ~ptr:ptr0 ~blks:()
      in
      Unix.close fd;
      ess

    let _ = read_back
  end

end
(* open Blk_layer *)


(* FIXME perhaps parameterize the blk_layers over blk_ops, blk_dev_ops, monad_ops *)
module Lwt_blk_layer = struct
  let blk_ops = Common_blk_ops.String_.make ~blk_sz

  let blk_dev_ops ~fd =
    Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd
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

  let pl_state_ops = Pl_impl.pl_state_ops
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
end





(* known at compile time *)
module type S0 = sig
  type t
  val monad_ops: t monad_ops

end


module type S1 = sig
  include S0

  type k
  val compare: k -> k -> int
  type v

  type ptr = blk_id
  val config: (k,v,ptr)Config.config
  val ptr0: ptr
end

module type S2 = sig
  include S1

  open Pl_impl
  val with_pl: (pl_internal_state,t) with_state
  open Pcl_impl
  val with_pcl : (pcl_internal_state, t) with_state

  (* open Dcl_types *)
  (* val with_dmap: ((ptr, (k,v,unit)Tjr_map.map) dcl_state, t)with_state *)
  val with_dmap: ((ptr, k,v)Dmap_types.dmap_state, t)with_state

  val alloc: unit -> (ptr,t)m
end


(* probably only known at runtime *)
module type S3 = sig
  include S2

  (* type file_descr *)
  (* val fd: file_descr *)
  type blk = string
  val blk_dev_ops : (blk_id,blk,t)blk_dev_ops
end

module Make(S:S3) : sig
val dmap_ops : (S.k, S.v, S.ptr, S.t) Generic_make_functor.dmap_with_sync end
 = struct
  open S

  (* let blk_dev_ops = blk_dev_ops ~monad_ops ~fd *)

  let write_count = ref 0

  let _ = Pervasives.at_exit (fun () ->
      Printf.printf "Blk write count: %d\n" !write_count)

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  (* we assume the fd is fixed *)
  let write_node (pl_state: Pl_impl.pl_internal_state) =
    (* mark "start"; *)
    let buf,pos = pl_state.data in
    let eol = End_of_list pl_state.next in
    let _pos = (elt_writer ~config).write buf ~pos eol in
    (* FIXME could be more efficient if we wrote direct to disk without
       going via string *)
    (* mark "buf2string"; *)
    let blk = Core.Bigstring.to_string buf in
    (* mark "buf2string'"; *)
    blk_dev_ops.write ~blk_id:pl_state.current ~blk >>= fun x ->
    incr write_count;
    (* mark "end";  *)
    return x

  module Internal = struct
    type k = S.k
    type v = S.v
    type nonrec ptr = blk_id
    type t = S.t
    let monad_ops = S.monad_ops


    include Pl_impl
    (* let pl_state_ops = Pl_impl.pl_state_ops *)

    include Pcl_impl
    let pcl_state_ops = Pcl_impl.pcl_state_ops ~config

    type e = (k,v) Ins_del_op.op

    (* let with_pl,_with_pcl,_with_dmap = S.(with_pl,with_pcl,with_dmap) *)
  end

  module G = Tjr_pcache.Generic_make_functor.Make(Internal)

  let dmap_ops : (k, v, ptr, t) Generic_make_functor.dmap_with_sync =
    G.make_dmap_ops ~alloc ~with_pl ~write_node ~with_pcl ~with_dmap
end


(** Specialize to store-passing monad *)
module With_fstore = struct

  open Pcache_store_passing

  module Make(S:S1 with type t = fstore_passing)
    : sig   
      val make_dmap_ops :
        fn:string ->
        (Unix.file_descr * fstore,
         (S.k, S.v, blk_id, fstore_passing) Generic_make_functor.dmap_with_sync)
          initial_state_and_ops
    end
  = struct
    open S


    open Pcache_store_passing

    (** Track the various bits of state by using a functional store:

        - free (free block ptr)
        - fd (block device via file descriptor)
        - pl
        - pcl
        - dmap

    *)
    module Fstore = struct
      module S_ = Simple_pl_and_pcl_implementations

      (* NOTE we allow_reset, but only for fd_ref (and free ref?) *)
      module R = Tjr_store.Make_imperative_fstore(struct let allow_reset=true end)

      (* initial block number *)
      let free_ref = R.ref (config.next_free_ptr ptr0)
      let pl_ref = R.ref S_.Pl_impl.{data=(create_buf blk_sz_as_int,0); current=ptr0; next=None} 
      let pcl_ref = R.ref Pcl_impl.{pcl_data=(create_buf blk_sz_as_int,0)}
      let dmap_ref = R.ref @@
        (* FIXME this should be elsewhere *)
        let kvop_map_ops = Op_aux.default_kvop_map_ops () in
        let empty_map = kvop_map_ops.empty in
        Pcache_intf.Dcl_types.{ 
          start_block=ptr0;
          current_block=ptr0;
          block_list_length=1;
          abs_past=empty_map;
          abs_current=empty_map }

      let with_free = with_ref free_ref
      let with_pl = with_ref pl_ref
      let with_pcl = with_ref pcl_ref
      let with_dmap = with_ref dmap_ref
(*
    let fd_ref = R.ref fd
    let with_fd = with_ref fd_ref
*)

      let alloc () = with_free.with_state (fun ~state:free ~set_state ->
          let free' = free |> Blk_id.to_int |> fun x -> x+1 |> Blk_id.of_int in
          set_state free' >>= fun () ->
          return free)
    end
    (* open Fstore *)



    let make_dmap_ops ~fn :
      (Unix.file_descr * fstore,
       (S.k, S.v, blk_id, S.t) Generic_make_functor.dmap_with_sync)
        initial_state_and_ops
      =
      let fd = Tjr_file.fd_from_file ~fn ~create:true ~init:true in
      let blk_dev_ops = Unix_blk_layer.blk_dev_ops ~monad_ops ~fd in
      let module Internal = struct
        include S
        include Fstore
        type blk=string 
        let blk_dev_ops = blk_dev_ops
        let alloc = alloc
      end
      in
      let module A = Make(Internal) in
      let initial_store = !Fstore.R.fstore in
      {initial_state=(fd,initial_store);ops=A.dmap_ops}

  end


  (** Construct the dmap example backed by a file, using the fstore interface *)
  let make_dmap_on_file (type k v) ~compare ~config ~ptr0 ~fn =
    let module A = struct
      type t = fstore_passing
      let monad_ops=monad_ops
      type nonrec k=k
      let compare=compare
      type nonrec v=v
      type ptr=blk_id
      let config=config
      let ptr0=ptr0
    end
    in
    let module B = Make(A) in
    B.make_dmap_ops ~fn

  let _ = make_dmap_on_file

  let ptr0 = Blk_id.of_int 0

  module Common_dmap_on_file_instances = struct

    let int_int ~fn =
      make_dmap_on_file ~compare:(Int_.compare) ~config:Config.int_int_config
        ~ptr0 ~fn

    (* FIXME other instances here *)
  end

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



(** Specialize to lwt monad *)
module With_lwt = struct

  open Tjr_monad.With_lwt           

  module Make(S:S1 with type t = Tjr_monad.With_lwt.lwt)
    : sig   
      open S
      val make_dmap_ops :
        fn:string ->
(Lwt_unix.file_descr *
 (ptr ref *
  (buf * int, ptr) Simple_pl_and_pcl_implementations.Pl_impl.pl_state ref *
  Pcl_internal_state.pcl_internal_state ref *
  (ptr, (k, (k, v) Ins_del_op.op, unit) Tjr_map.map) Dcl_types.dcl_state ref),
 (k, v, ptr, lwt) Generic_make_functor.dmap_with_sync)
initial_state_and_ops
    end
  = struct
    open S


    (* An imperative store to hold state for lwt *)
    module Store = struct
      module S_ = Simple_pl_and_pcl_implementations

      (* initial block number *)
      let free_ref = ref (config.next_free_ptr ptr0)
      let pl_ref = ref 
          S_.Pl_impl.{data=(create_buf blk_sz_as_int,0); current=ptr0; next=None} 
      let pcl_ref = ref Pcl_impl.{pcl_data=(create_buf blk_sz_as_int,0)}
      let dmap_ref = ref @@
        (* FIXME this should be elsewhere *)
        let kvop_map_ops = Op_aux.default_kvop_map_ops () in
        let empty_map = kvop_map_ops.empty in
        Pcache_intf.Dcl_types.{ 
          start_block=ptr0;
          current_block=ptr0;
          block_list_length=1;
          abs_past=empty_map;
          abs_current=empty_map }

      let with_ref r = 
        let with_state f = 
          return () >>= fun () -> 
          let state = !r in
          let set_state r' = r:=r'; return () in
          f ~state ~set_state
        in
        { with_state }        

      let with_free = with_ref free_ref
      let with_pl = with_ref pl_ref
      let with_pcl = with_ref pcl_ref
      let with_dmap = with_ref dmap_ref
          
      let alloc () = with_free.with_state (fun ~state:free ~set_state ->
          let free' = free |> Blk_id.to_int |> fun x -> x+1 |> Blk_id.of_int in
          set_state free' >>= fun () ->
          return free)
    end



    let make_dmap_ops ~fn
      : (Lwt_unix.file_descr *
 (ptr ref *
  (buf * int, ptr) Simple_pl_and_pcl_implementations.Pl_impl.pl_state ref *
  Pcl_internal_state.pcl_internal_state ref *
  (ptr, (k, (k, v) Ins_del_op.op, unit) Tjr_map.map) Dcl_types.dcl_state ref),
 (k, v, ptr, lwt) Generic_make_functor.dmap_with_sync)
initial_state_and_ops
      =
      let fd = Tjr_file.fd_from_file ~fn ~create:true ~init:true in
      let fd = Lwt_unix.of_unix_file_descr fd in
      let blk_dev_ops = Lwt_blk_layer.blk_dev_ops ~fd in
      let module Internal = struct
        include S
        include Store
        type blk=string 
        let blk_dev_ops = blk_dev_ops
        let alloc = alloc
      end
      in
      let module A = Make(Internal) in
      let initial_store = Store.(free_ref,pl_ref,pcl_ref,dmap_ref) in
      {initial_state=(fd,initial_store);ops=A.dmap_ops}

  end


  (** Construct the dmap example backed by a file, using the fstore interface *)
  let make_dmap_on_file (type k v) ~compare ~config ~ptr0 ~fn =
    let module A = struct
      type t = lwt
      let monad_ops=lwt_monad_ops
      type nonrec k=k
      let compare=compare
      type nonrec v=v
      type ptr=blk_id
      let config=config
      let ptr0=ptr0
    end
    in
    let module B = Make(A) in
    B.make_dmap_ops ~fn

  let _ = make_dmap_on_file

  let ptr0 = Blk_id.of_int 0

  module Common_dmap_on_file_instances = struct

    let int_int ~fn =
      make_dmap_on_file ~compare:(Int_.compare) ~config:Config.int_int_config
        ~ptr0 ~fn

    (* FIXME other instances here *)
  end

end



module Pcache_layers = struct
  open Pl_types
  open Pcl_types
  open Dmap_types

  (** NOTE in the following type, the option refs have to be
      initialized before calling dmap_ops *)
  type ('k,'v,'ptr,'t,'blk,'pl_data,'pl_internal_state,'pcl_internal_state,'fd,'e) pcache_layers = {
    monad_ops     : 't monad_ops;

    config        : ('k,'v,'ptr) Config.config;
    elt_writer    : ('k,'v,'ptr) Pcl_elt.elt Bin_prot.Type_class.writer;
    elt_reader    : ('k,'v,'ptr) Pcl_elt.elt Bin_prot.Type_class.reader;

    blk_ops       : 'blk blk_ops;
    blk_dev_ops   : 'fd -> ('ptr,'blk,'t)blk_dev_ops;
    read_back     : blk_dev_ops:('ptr,'blk,'t)blk_dev_ops -> root_ptr:'ptr -> 'e list list;
    (* NOTE 'e is likely ins/del, different from pcl_elt.elt *)

    pl_state_ops  : ('pl_data,'ptr,'pl_internal_state)pl_state_ops;
    pcl_state_ops : ('pl_data,('k,'v)op,'pcl_internal_state)pcl_state_ops;

    alloc         : (unit -> ('ptr,'t)m)option ref;
    with_pl       : ('pl_internal_state,'t) with_state option ref;
    with_pcl      : ('pcl_internal_state,'t) with_state option ref;
    with_dmap     : (('ptr,'k,'v)dmap_state,'t) with_state option ref;

    (* FIXME why does dmap_ops need pl write_node? *)
    dmap_ops      : write_node:('pl_internal_state -> (unit,'t)m) ->  ('k,'v,'ptr,'t)dmap_ops
  }

end
open Pcache_layers


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

  let _ = read_node


  module Int_int = struct
    type k = int
    type v = int
    let config = Config.int_int_config
    let elt_writer = Pcl_elt.elt_writer ~config
    let elt_reader = Pcl_elt.elt_reader ~config
  end


  module With_lwt = struct
    
    let monad_ops = lwt_monad_ops
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    type t = lwt
      
    let blk_dev_ops fd = Lwt_blk_layer.blk_dev_ops ~fd


    let read_back ~blk_dev_ops ~config ~ptr = 
      let read_node ptr _blks = read_node ~monad_ops ~config ~blk_dev_ops ~blk_id:ptr in
      let _ = read_node in
      let ess =
        Persistent_chunked_list.pcl_to_elt_list_list  FIXME we need this to be in the monad
          ~read_node ~ptr:ptr0 ~blks:()
      in
      Unix.close fd;
      ess


    let make_int_int_layers () = 
      let open Int_int in
      let module A = struct
        module S = Int_int
                
        let pcl_state_ops = Pcl_impl.pcl_state_ops ~config
        let _ = pcl_state_ops
                              
        let alloc = ref None
        let with_pl = ref None
        let with_pcl = ref None
        let with_dmap = ref None

        let dmap_ops ~write_node = 
          assert(
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
            | _ -> true);
          (* FIXME might also have a version where with_pl etc are specialized to stdlib.refs *)
          let Some alloc,Some with_pl,Some with_pcl,Some with_dmap = 
            (!alloc),(!with_pl),(!with_pcl),(!with_dmap) [@@ocaml.warning "-8"]
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

            type nonrec pcl_internal_state = Pcl_internal_state.pcl_internal_state
            type e = (k,v) Pcache_intf.Ins_del_op.op
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
        blk_ops;
        blk_dev_ops;
        read_back;
        pl_state_ops;
        pcl_state_ops;
        alloc;
        with_pl;
        with_pcl;
        with_dmap;
        dmap_ops
      }
  end

end

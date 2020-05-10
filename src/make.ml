(** Pcache constructor functions, including on-disk format etc *)


(** On-disk format:

|op1|...|opn|0|...|nxt:r option|

The 0 byte marks the end of the op list (eol); op never starts with 0 byte.
*)

open Pcache_intf

(** Make functor argument sig, including types and values *)
module type S = sig
  type t
  val monad_ops: t monad_ops

  type k
  type v
  type r
  type blk_id = r
  type blk

  val blk_ops: blk blk_ops
  val k_cmp: k -> k -> int

  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
  val r_mshlr: r bp_mshlr

end

(** Make functor return sig *)
module type T = sig
  type k
  type v
  type r
  type t

  type kvop_map
  
  val pcache_factory: 
    blk_alloc:(unit -> (r, t) m) -> 
    (k, v, r, buf, kvop_map, t) pcache_factory

end


(**/**)
(** For testing *)
module Pvt_chk = struct
  let check_buf_pos ~buf ~buf_pos = 
    assert( (buf_ops.get buf_pos buf = chr0) || (Printf.printf "Erk!: %d %c\n" buf_pos (buf_ops.get buf_pos buf); false) )

  let check_state s = 
    check_buf_pos ~buf:s.buf ~buf_pos:s.buf_pos

end
open Pvt_chk
(**/**)

(* and type pcache_state=(S.r,(S.k,S.v)kvop)pcache_state *)
module Make(S:S) : T with type k=S.k and type v=S.v and type r=S.r and type t=S.t = struct
  open S
  type k = S.k
  type v = S.v
  type r = S.r
  type t = S.t
  (* type pcache_state=S.pcache_state *)

  module Kvop_map = Kvop_map.Make(struct
      type k = S.k
      type v = S.v
      let k_cmp = k_cmp
    end)
  let kvop_map_ops = Kvop_map.map_ops
  (* let empty_kvop_map = kvop_map_ops.empty *)
  type kvop_map = Kvop_map.t
  (* type nonrec kvop = (k,v)kvop *)
      
  type nonrec pcache_state = (r,kvop_map)pcache_state
  (* let empty_pcache_state ~r = Pcache_state.empty_pcache_state ~root_ptr:r ~current_ptr:r ~empty:empty_kvop_map *)

  let return = monad_ops.return
  let ( >>= ) = monad_ops.bind

  let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int

  module K = (val k_mshlr)
  module V = (val v_mshlr)
  module R = (val r_mshlr)


(*
  module S = (val marshalling_config : 
               MRSHL with type k=k and type v=v and type r=r)
  open S
*)

  let eol_sz = 1

  (** NOTE we use the fact that None marshalls to the 0 byte *)
  module Nxt = struct 
    open Bin_prot.Std
    type nxt = R.t option [@@deriving bin_io]
    let nxt_size = R.max_sz + 1  (* +1 for option *)
    (* gives bin_reader_nxt and bin_writer_nxt *)
  end
  open Nxt

  (* check that None marshalls to the empty byte *)
  let _ : unit = 
    buf_ops.create blk_sz |> fun buf ->
    (*  Buffers are zero-ed on creation  for i = 0 to blk_sz-1 do
          assert(buf_ops.get i buf = chr0)
        done; *)
    bin_write_nxt buf ~pos:0 None |> fun n ->
    assert (n=1);
    assert (Char.equal (buf_ops.get 0 buf) chr0)

  (* let next_ptr_byte_size = r_size *)
      
  let op_byte_size = K.max_sz + V.max_sz + 1

  (* FIXME include type in tjr_fs_shared *)

  module Read_pcache = struct
    let nxt_pos = (blk_sz - nxt_size)

    (** We introduce an "extended op" to ensure that no op constructor is represented as a 0 byte *)
    module X = struct
      type t = Nil | Insert of (K.t*V.t) | Delete of K.t [@@deriving bin_io]
    end

    let buf_to_op_list_x_nxt_x_pos buf =
      let pos_ref = ref 0 in
      let rec f xs = 
        match buf_ops.get !pos_ref buf = chr0 with 
        | true -> List.rev xs
        | false -> 
          X.bin_reader_t.read buf ~pos_ref |> fun xop ->
          Kvop.(match xop with X.Insert(k,v) -> Insert(k,v) | X.Delete k -> Delete k | Nil -> failwith __LOC__) |> fun op ->
          f (op::xs)
      in
      f [] |> fun ops ->
      let nxt = bin_reader_nxt.read buf ~pos_ref:(ref nxt_pos) in
      (ops,nxt,!pos_ref)


    let read_pcache ~root ~read_blk_as_buf 
      : ( <tl: ((k,v)kvop list * r option) list;
           hd:  (r * buf * int) >,'t)m =
      read_blk_as_buf root >>= fun buf ->
      ([],root,buf) |> iter_k (fun ~k (acc,r,buf) ->
          let ops,nxt,pos = buf|>buf_to_op_list_x_nxt_x_pos in
          match nxt with
          | None -> return (object
                      method tl=List.rev ((ops,nxt)::acc)
                      method hd=(r,buf,pos) end)
          | Some r -> 
            read_blk_as_buf r >>= fun buf ->
            k ( (ops,Some r)::acc, r, buf))
          
    let _ :
root:r ->
read_blk_as_buf:(r -> (Tjr_pcache__.Pcache_intf.buf, t) Tjr_monad.m) ->
(< hd : r * Tjr_pcache__.Pcache_intf.buf * int;
   tl : ((k, v) Tjr_fs_shared.kvop list * r option) list >,
 t)
Tjr_monad.m
      = read_pcache        
  end
  open Read_pcache

  module Pvt(S2:sig
      val blk_alloc: unit -> (r,t) m
      val with_pcache: (pcache_state,t)with_state          
      val flush_tl: pcache_state -> (unit,t)m
    end) = struct
    open S2

    let find k = with_pcache.with_state (fun ~state ~set_state:_ -> 
        kvop_map_ops.find_opt k state.current_map |> function
        | Some op -> (
            match (op:(k,v)kvop) with
            | Insert(_,v) -> return (Some v)
            | Delete _ -> return None)
        | None -> (
            kvop_map_ops.find_opt k state.past_map |> function
            | Some(Kvop.Insert(_,v)) -> return (Some v)
            | Some(Delete _) -> return None
            | None -> return None))

    (* we write the next ptr at position buf_sz - next_ptr_byte_size *)

    (* when next_ptr gets set, it automatically gets marshalled into last nxt_size bytes of buf *)
    let set_next_ptr r s = 
      assert(s.next_ptr=None);
      let { buf; _ } = s in
      let _n : int = 
        bin_writer_nxt.write buf ~pos:nxt_pos (Some r)
      in
      { s with next_ptr=(Some r); buf; dirty=true }    

    let alloc_next_ptr_and_set s = 
      assert (s.next_ptr = None);
      blk_alloc () >>= fun r -> 
      return (r,set_next_ptr r s)

(*
  let alloc_next_ptr_if_none = 
    with_pcache.with_state (fun ~state:s ~set_state ->
      match s.next_ptr with
      | None -> alloc_next_ptr_and_set s >>= fun (r,s) -> 
                set_state s
      | Some _ -> return ())
*)
(*
  let get_dirty () =     
    with_pcache.with_state (fun ~state:s ~set_state:_ ->
      return s.dirty)
*)
(*
  let write_if_dirty = 
    with_pcache.with_state (fun ~state:s ~set_state:_ ->
      match s.dirty with
      | true -> flush_tl s
      | false -> return ())
*)    


    let can_fit n state = state.buf_pos + n < (blk_sz - nxt_size) - eol_sz  (* we need 1 byte for end of list marker *)

    let add_in_current_node op state =
      assert(can_fit op_byte_size state);
      check_state state;
      (* write into buf, and adjust the state *)
      let { current_map; buf; buf_pos; dirty; _ } = state in
      let current_map = 
        match (op:(k,v)kvop) with
        | Insert(k,v) -> kvop_map_ops.add k op current_map 
        | Delete k -> kvop_map_ops.add k op current_map
      in
      assert( (buf_ops.get buf_pos buf = chr0) || (Printf.printf "Erk!: %d %c\n" buf_pos (buf_ops.get buf_pos buf); false) );
      let buf_pos =
        let n = 
          X.bin_writer_t.write buf ~pos:buf_pos (match op with
              | Insert (k,v) -> X.Insert(k,v)
              | Delete k -> X.Delete k)
          (* note this mutates! *)
        in
        n
      in
      (* NOTE we assume that the eol byte is already 0, since the block was newly created *)
      assert( (buf_ops.get buf_pos buf = chr0) || (Printf.printf "Erk!: %d %c\n" buf_pos (buf_ops.get buf_pos buf); false) );
      let dirty = true in
      { state with current_map; buf; buf_pos; dirty }

    let add op = 
      with_pcache.with_state (fun ~state ~set_state -> 
          check_state state;
          (* can we insert in the current node? *)
          match can_fit op_byte_size state with
          | true -> (
              add_in_current_node op state |> fun state -> 
              check_state state;
              set_state state)
          | false -> (
              (* we need to allocate a new node if not already allocated,
                 issue a write for the current node then move to the next *)
              alloc_next_ptr_and_set state >>= fun (r,state) ->
              flush_tl state >>= fun () ->
              let buf = buf_ops.create blk_sz in
              (* for i = 0 to blk_sz-1 do
                 assert(buf_ops.get i buf = chr0)
                 done; *)
              let state = { root_ptr=state.root_ptr;
                            past_map=Tjr_map.map_merge ~map_ops:kvop_map_ops ~old:state.past_map ~new_:state.current_map;
                            current_ptr=r;
                            current_map=kvop_map_ops.empty;
                            buf;
                            buf_pos=0;
                            next_ptr=None;
                            blk_len=state.blk_len+1;
                            dirty=true }
              in
              (* Printf.printf "buf_pos: %d\n" state.buf_pos; *)
              add_in_current_node op state |> fun state ->
              check_state state;
              set_state state))

    let insert k v = add (Insert(k,v))
    let delete k = add (Delete k)

    let detach () = 
      with_pcache.with_state (fun ~state ~set_state ->
          assert(state.blk_len >= 1);
          match state.blk_len with
          | 1 -> (
              (* the case when there is only one block *)
              let { root_ptr; past_map; current_ptr; current_map; _ } = state in
              return { root_ptr; past_map; current_ptr; current_map })
          | _ -> (
              let { root_ptr; past_map; current_ptr; current_map; _ } = state in
              let detach_info = { root_ptr; past_map; current_ptr; current_map } in          
              let state = { state with root_ptr=current_ptr;
                                       past_map=kvop_map_ops.empty;
                                       blk_len=1 }
              in
              check_state state;
              set_state state >>= fun () -> 
              return detach_info))

    let blk_len () = with_pcache.with_state (fun ~state:s ~set_state:_  ->
        return s.blk_len)

    let pcache_write () = with_pcache.with_state (fun ~state:s ~set_state:_  ->
        flush_tl s)

    let pcache_sync = pcache_write

    let pcache_ops = { find; insert; delete; detach; blk_len; pcache_write; pcache_sync }
  end

  type nonrec pcache_ops = (k,v,r,kvop_map,t) pcache_ops    

  let read_pcache = Read_pcache.read_pcache

  let _ = read_pcache

  let make_pcache_ops  
      ~(blk_alloc: unit -> (r,t) m)
      ~(with_pcache: (pcache_state,t)with_state)
      ~(flush_tl: pcache_state -> (unit,t)m)
    : pcache_ops
    = 
    let module P = Pvt(struct let blk_alloc=blk_alloc let with_pcache=with_pcache let flush_tl=flush_tl end) in
    P.pcache_ops

  (* FIXME current_ptr is not needed - we get this from read_pcache *)
  let read_initial_pcache_state ~read_blk_as_buf ~root_ptr = 
    read_pcache ~root:root_ptr ~read_blk_as_buf >>= fun o ->
    (* (xs,buf,buf_pos) *)
    let (current_ptr,buf,buf_pos) = o#hd in
    let xs = o#tl in
    assert(List.length xs >= 1);
    let xs : ((k,v)kvop list * r option) list = xs in
    let past_map,last = ([],xs,kvop_map_ops.empty) |> iter_k (fun ~k:kont x ->
        match x with
        | [],[],acc -> failwith "impossible"
        | [],[last],acc -> acc,fst last
        | op::xs,ys,acc -> 
          let acc = kvop_map_ops.add (Kvop.op2k op) op acc in
          kont (xs,ys,acc)
        | [],y::ys,acc -> kont (fst y,ys,acc))
    in
    let current_map = last |> List.map (fun op -> (Kvop.op2k op,op)) |> kvop_map_ops.of_bindings in
    return {
      root_ptr;
      past_map;
      current_ptr;
      current_map;
      buf;
      buf_pos;
      next_ptr=None;
      blk_len=List.length xs;
      dirty=false
    }          

  (* FIXME this requires blk=buf=ba_buf *)
  let make_pcache_ops_with_blk_dev 
      ~(blk_dev_ops: (blk_id,ba_buf,t)blk_dev_ops)
      ~(blk_alloc: unit -> (r,t) m)
      ~(with_pcache: (pcache_state,t)with_state)
    : pcache_ops
    =
    let flush_tl s = 
      (* Printf.printf "Writing to disk with next pointer %d\n" (dest_Some s.next_ptr |> Blk_id.to_int);  *)
      blk_dev_ops.write ~blk_id:s.current_ptr ~blk:s.buf
    in
    make_pcache_ops ~blk_alloc ~with_pcache ~flush_tl

  let _ = make_pcache_ops_with_blk_dev

(*
  let with_ref r = 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun s -> r:=s; return ()) in
    { with_state }

  let make_as_obj () : (k,v,r,'blk,kvop_map,t) pcache_as_obj = 
    let open (struct 
      let blk_alloc = ref None
      (* let blk_dev_ops = ref None  *)
      let flush_tl = ref None 
      let with_state = ref None 
      let set_blk_alloc x = blk_alloc:=Some x
      let set_initial_state x = 
        let x = ref x in
        let wr = with_ref x in
        with_state:=Some wr
      let set_flush_tl x = flush_tl:=Some x
      let set_blk_dev_ops (blk_dev_ops:('r,'blk,'t)blk_dev_ops) =
        let flush_tl s = 
          (* Printf.printf "Writing to disk with next pointer %d\n" (dest_Some s.next_ptr |> Blk_id.to_int);  *)
          blk_dev_ops.write ~blk_id:s.current_ptr ~blk:s.buf
        in
        set_flush_tl flush_tl      
      let check_initialized () =
        let open Option in
        assert(is_some !blk_alloc);
        assert(is_some !flush_tl);
        assert(is_some !with_state);
        ()
      let kvop_map_ops () = kvop_map_ops
      let get_with_pcache () = 
        check_initialized();
        Option.get !with_state
      let get_pcache_ops () = 
        check_initialized ();
        make_pcache_ops 
          ~blk_alloc:(Option.get !blk_alloc)
          ~with_pcache:(Option.get !with_state)
          ~flush_tl:(Option.get !flush_tl)
    end)
    in
    object 
      method set_blk_alloc=set_blk_alloc
      method set_initial_state=set_initial_state
      method set_flush_tl=set_flush_tl
      method set_blk_dev_ops=set_blk_dev_ops
      method check_initialized=check_initialized
      method kvop_map_ops=kvop_map_ops
      method get_with_pcache=get_with_pcache
      method get_pcache_ops=get_pcache_ops
    end

  let _ = make_as_obj
*)
    
  let pcache_factory ~(blk_alloc:unit -> (r,t)m) : (k,v,r,buf,kvop_map,t) pcache_factory 
    = 
    object (self)
      method kvop_map_ops = kvop_map_ops

      method empty_pcache_state = fun ~root_ptr ~current_ptr ->
        empty_pcache_state ~root_ptr ~current_ptr ~empty:(self#kvop_map_ops.empty)

      method with_read_blk_as_buf = 
        fun ~read_blk_as_buf ~flush_tl -> 
        object (self1)
          method read_pcache = fun root -> 
            Read_pcache.read_pcache ~root ~read_blk_as_buf >>= fun o -> 
            return o
            (* return (object method tail=tail method hd=(hd,i) end) *)

          method read_initial_pcache_state = fun root ->
            read_initial_pcache_state ~read_blk_as_buf ~root_ptr:root 

          method make_pcache_ops = object (self2)
            method with_state = fun with_pcache -> 
              make_pcache_ops ~blk_alloc ~with_pcache ~flush_tl

            method from_root = fun r -> 
              self1#read_initial_pcache_state r >>= fun pc -> 
              let r = ref pc in
              let with_pcache = Tjr_monad.with_imperative_ref ~monad_ops r in
              return (object
                method with_state=with_pcache 
                method pcache_ops=(self2#with_state with_pcache)
              end)
          end
        end

      method with_blk_dev_ops ~blk_dev_ops = 
        let flush_tl s = 
          (* Printf.printf "Writing to disk with next pointer %d\n"
             (dest_Some s.next_ptr |> Blk_id.to_int); *)
          blk_dev_ops.write ~blk_id:s.current_ptr ~blk:s.buf
        in
        let read_blk_as_buf r = blk_dev_ops.read ~blk_id:r in
        self#with_read_blk_as_buf ~read_blk_as_buf ~flush_tl

    end

  let _ = pcache_factory

end (* Make *)

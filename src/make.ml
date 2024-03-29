(** Pcache constructor functions, using plist *)

open Pcache_intf

let plist_to_pcache
    ~(kvop_map_ops : _ Tjr_map.map_ops)
    ~monad_ops
    ~(simple_plist_ops: (('k,'v)kvop,'r,'t)simple_plist_ops)
    ~(with_state : (('r,'kvop_map) pcache_state,'t) with_state)
    : _ pcache_ops
  =
  let open (struct
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    let Tjr_map.{find_opt; add; of_bindings; empty; _ } = kvop_map_ops

    let merge ~old ~new_ = Tjr_map.map_merge ~map_ops:kvop_map_ops ~old ~new_

    let with_state = with_state.with_state

    let kvop_to_v = Kvop.(function
      | Insert(k,v) -> Some v
      | Delete _ -> None)

    let find k =
      with_state (fun ~state ~set_state ->
          find_opt k state.current_map |> function
          | Some op -> kvop_to_v op |> return
          | None ->
            find_opt k state.past_map |> function
            | Some op -> kvop_to_v op |> return
            | None -> return None)

    let insert k v =
      with_state (fun ~state ~set_state ->
          simple_plist_ops.add (Insert(k,v)) >>= function
          | false ->
            set_state {state with current_map=(add k (Insert(k,v)) state.current_map)}
          | true ->
            simple_plist_ops.get_origin () >>= fun o ->
            let past_map = merge ~old:state.past_map ~new_:state.current_map in
            let current_map = of_bindings [(k,Insert(k,v))] in
            let current_ptr = o.tl in
            set_state {root_ptr=state.root_ptr; past_map; current_ptr; current_map})

    let delete k =
      with_state (fun ~state ~set_state ->
          simple_plist_ops.add (Delete k) >>= function
          | false ->
            set_state {state with current_map=(add k (Delete k) state.current_map)}
          | true ->
            simple_plist_ops.get_origin () >>= fun o ->
            let past_map = merge ~old:state.past_map ~new_:state.current_map in
            let current_map = of_bindings [(k,Delete k)] in
            let current_ptr = o.tl in
            set_state {root_ptr=state.root_ptr; past_map; current_ptr; current_map})

    let detach () =
      with_state (fun ~state ~set_state ->
          let Pcache_state.{root_ptr;past_map;current_ptr;current_map} = state in
          let to_return = Detach_info.{root_ptr;past_map;current_ptr;current_map} in
          (* FIXME just identify detach info with pcache_state? *)
          set_state { root_ptr=current_ptr;past_map=empty;current_ptr;current_map } >>= fun () ->
          return to_return)

    let blk_len () = simple_plist_ops.blk_len ()

    let get_origin () = simple_plist_ops.get_origin ()

    let pcache_sync () = 
      Printf.printf "%s: pcache_sync called\n%!" __FILE__;
      simple_plist_ops.sync_tl ()

  end)
  in
  Pcache_ops.{ find; insert; delete; detach; blk_len; get_origin; pcache_sync }


open Shared_ctxt

module type S = sig
    type k
    val k_cmp: k->k->int
    type v
    type a = (k,v)kvop
    val simple_plist_factory: (a,blk_id,blk,buf,t) simple_plist_factory
end

module type T = sig
  module S : S
  open S
  type kvop_map
  val pcache_factory : (a,k,v,blk_id,blk,buf,kvop_map,t) pcache_factory
(* NOTE this is specific to shared_ctxt; we could generalize further
   if needed *)
end

module Make_example(S:S) : T with module S=S = struct
  module S = S
  open S

  let note_these_types_are_equal=(fun (a:a) (b:(k,v)kvop) -> ())  

  module Map = Tjr_map.Make_map_ops(struct 
      type nonrec k = k type nonrec v = (k,v)kvop let k_cmp = k_cmp end)

  type kvop_map = Map.t

  let kvop_map_ops = Map.map_ops
      
  let empty_pcache r = Pcache_intf.empty_pcache_state ~ptr:r ~empty:kvop_map_ops.empty

  let simple_plist_factory = simple_plist_factory

  let plist_to_pcache ~simple_plist_ops ~with_state = 
      plist_to_pcache
        ~kvop_map_ops
        ~monad_ops
        ~simple_plist_ops
        ~with_state    

  module With_(A:sig
      val blk_dev_ops  : (r,blk,t)blk_dev_ops
      val barrier      : (unit -> (unit,t)m)  
      val freelist_ops : (r,t) freelist_ops_af
    end) = struct
    open A
        
    let plist_factory = simple_plist_factory#plist_factory

    let pl_with = plist_factory#with_blk_dev_ops ~blk_dev_ops ~barrier

    (* let spl_with = simple_plist_factory#with_ ~blk_dev_ops ~barrier ~freelist_ops *)

    let create () =
      freelist_ops.blk_alloc () >>= fun b_pcache ->
      (* let simple_plist_factory = simple_plist_factory in *)
      pl_with#create b_pcache >>= fun created_pl ->
      let plist_ops = created_pl#plist_ops in
      created_pl#plist_ops.get_origin () >>= fun pl_origin ->
      let simple_plist_ops = 
        simple_plist_factory#convert_to_simple_plist ~freelist_ops ~plist_ops in
      let pc_ref = ref (empty_pcache b_pcache) in
      let with_state = with_imperative_ref ~monad_ops pc_ref in
      let pcache_ops = plist_to_pcache ~simple_plist_ops ~with_state in
      return pcache_ops

    let ops_to_map ops = 
      (kvop_map_ops.empty,ops) |> iter_k (fun ~k:kont (map,xs) -> 
          match xs with
          | [] -> map
          | op::xs -> Kvop.(
              match op with
              | Insert(k,v) -> 
                kont (kvop_map_ops.add k op map, xs)
              | Delete k -> 
                kont (kvop_map_ops.add k op map, xs)))

    let restore ~(hd:blk_id) = 
      (* we read the plist from the hd, convert to past and current maps *)
      pl_with#init#read_from_hd hd >>= fun blks -> 
      (hd,0,blks,kvop_map_ops.empty) |> iter_k (fun ~k:kont (ptr,len,blks,map) -> 
          (* NOTE ptr is a ptr to the first blk in blks *)
          match blks with 
          | [] -> failwith "impossible: list is not nil and we return when tl is []" 
          | [(ops,nxt)] -> 
            assert(nxt=None);
            let past_map = map in
            let current_map = ops_to_map ops in
            let current_ptr = ptr in
            let len = len+1 in
            len,Pcache_state.{root_ptr=hd;past_map;current_ptr;current_map}
          | (ops,nxt)::blks -> 
            assert(nxt <> None);
            let m = ops_to_map ops in
            let map = Tjr_map.map_merge ~map_ops:kvop_map_ops ~old:map ~new_:m in
            let ptr = dest_Some nxt in
            let len = len+1 in
            kont (ptr,len,blks,map))
      |> fun (len,pc) -> 
      (* now reconstruct the plist *)
      pl_with#restore Pl_origin.{hd;tl=pc.current_ptr;blk_len=len} >>= fun pl_o -> 
      let simple_plist_ops = simple_plist_factory#convert_to_simple_plist ~freelist_ops ~plist_ops:pl_o#plist_ops in
      let pc_ref = ref pc in
      let with_state = with_imperative_ref ~monad_ops pc_ref in
      let pcache_ops = plist_to_pcache ~simple_plist_ops ~with_state in
      return pcache_ops

    let obj = object
      method create=create
      method restore=restore
    end
  end

  let with_ ~blk_dev_ops ~barrier ~freelist_ops = 
    let module A = With_(struct 
        let blk_dev_ops = blk_dev_ops 
        let barrier=barrier 
        let freelist_ops = freelist_ops 
      end) 
    in
    A.obj
      

  let pcache_factory : _ pcache_factory = object
    method empty_pcache=empty_pcache
    method note_these_types_are_equal=note_these_types_are_equal
    method kvop_map_ops=kvop_map_ops
    method simple_plist_factory=simple_plist_factory
    method plist_to_pcache=plist_to_pcache
    method with_=with_
  end
  
  let _ = pcache_factory
end


module Examples = struct
  
  (* int int example *)

  module Int_int = struct
    module S' = struct
      type k = int
      let k_cmp = Int.compare
      type v = int
      type a = (k,v)kvop
      let simple_plist_factory = simple_pl_examples#for_int_int_kvop
    end
    include Make_example(S')
  end

end

let examples = 
  object 
    method for_int_int : (_,int,int,_,_,_,_,_) pcache_factory = Examples.Int_int.pcache_factory
  end

let _ = examples




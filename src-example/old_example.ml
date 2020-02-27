(*
(** Pcache example *)

open Tjr_pcache.Pvt_make
open Pcache_intf.Pvt

module Make_with_fixed_types = Make_with_fixed_types

module Blk_id = Blk_id_as_int
type blk_id = Blk_id.blk_id [@@deriving bin_io, yojson]

module type MRSHL' = MRSHL with type k = int and type v = int and type r = blk_id

module Int_int_marshalling_config : MRSHL' = struct
  open Bin_prot.Std
  type k = int [@@deriving bin_io, yojson]
  type v = int [@@deriving bin_io, yojson]  
  type r = blk_id [@@deriving bin_io, yojson]
  type kvop = (k,v)Kvop.kvop [@@deriving bin_io, yojson]

  (** This is the max # of bytes required for k *)
  let k_size = 9
  let v_size = 9
  let r_size = 9  
end

let int_int_marshalling_config = (module Int_int_marshalling_config : MRSHL')

let make_kvop_map_ops () = 
  let map_ops = Kvop.default_kvop_map_ops () in
  let merge ~older ~newer = Tjr_map.map_merge ~map_ops ~old:older ~new_:newer in
  let Tjr_map.{ empty; find_opt; add; remove; _ } = map_ops in
  { empty; find_opt; insert=add; delete=remove; merge }

let _ = make_kvop_map_ops


type ('a,'b) make_t = {
  c_pcache: 'a;
  pcache_ops: 'b
}

(** Construct an int->int example *)
let make_generic ~blk_ops ~blk_alloc ~with_pcache ~flush_tl = 
  let kvop_map_ops = make_kvop_map_ops () in
  let c_pcache = { 
    monad_ops=lwt_monad_ops;
    marshalling_config=int_int_marshalling_config;
    kvop_map_ops;
    blk_ops;
    blk_alloc;
    with_pcache;
    flush_tl
  }
  in
  let pcache_ops = Tjr_pcache.Pvt_make.make c_pcache in
  {c_pcache; pcache_ops}

(* FIXME why is blk_ops generic here? *)
let _ :
blk_ops:'a blk_ops ->
blk_alloc:(unit -> (blk_id, lwt) m) ->
with_pcache:((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state, lwt)
          with_state ->
flush_tl:((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state ->
               (unit, lwt) m) ->
((lwt monad_ops, (module MRSHL'),
  (int, (int, int) kvop, (int, (int, int) kvop, 'b) Tjr_map.map)
  pcache_map_ops, 'a blk_ops, unit -> (blk_id, lwt) m,
  ((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state, lwt)
  with_state,
  (blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state ->
  (unit, lwt) m)
 c_pcache,
 (int, int, blk_id, (int, (int, int) kvop, 'b) Tjr_map.map, lwt) pcache_ops)
make_t
 = make_generic

;;

type ('a,'b,'c,'d) make_args = 
  | Make1: { blk_ops:'a; blk_alloc:'b; with_pcache:'c; flush_tl:'d } -> ('a,'b,'c,'d) make_args
  (** generic version *)


  | Make2: { flush_tl:'d } -> ('a,'x1,'x2,'d) make_args
  (** version with blk_ops std; pcache as a reference, and blk_alloc as a reference *)

[@@@ocaml.warning "-30"] (* duplicate record labes in mut rec type defn *)

type ('a,'b,'c) make_result = 
  | Res1: 'a aux1 -> ('a,'x1,'x2) make_result
  | Res2: ('a,'b,'c) aux2 -> ('a,'b,'c) make_result

and 'a aux1 = { pcache_ops:'a }

and ('a,'b,'c) aux2 = { pcache_ops: 'a; pcache_ref: 'b; min_free_blk:'c ref }

let dest_Res1 (Res1 x) = x[@@ocaml.warning "-8"]
let dest_Res2 (Res2 x) = x[@@ocaml.warning "-8"]


(** NOTE this takes Make1 to Res1, Make2 to Res2 etc *)     
let make = function
  | Make1 { blk_ops; blk_alloc; with_pcache; flush_tl } -> (
    make_generic ~blk_ops ~blk_alloc ~with_pcache ~flush_tl |> fun {pcache_ops; c_pcache=_ } ->
    Res1 { pcache_ops })
  | Make2 { flush_tl } -> 
    let module A = struct
      open Tjr_monad.With_lwt

      let blk_ops = Blk_factory.make_1()

      let min_free_blk = ref 1

      let blk_alloc () =     
        (* Printf.printf "blk_alloc: %d\n" !min_free_blk; *)
        let r = !min_free_blk  in
        incr min_free_blk;
        return (Blk_id.of_int r)

      let kvop_map_ops = Kvop.default_kvop_map_ops ()

      let blk_sz = 4096

      let pcache_state = ref {
          root_ptr=Blk_id.of_int 0;
          past_map=kvop_map_ops.empty;
          current_ptr=Blk_id.of_int 0;
          current_map=kvop_map_ops.empty;
          buf=buf_ops.create blk_sz;
          buf_pos=0;
          next_ptr=None;
          blk_len=1;
          dirty=true
        }

      let _ : unit = Chk.check_state (!pcache_state)

      let with_pcache = {
        with_state=fun f -> 
          f ~state:!pcache_state 
            ~set_state:(fun s -> 
              Chk.check_state s;
              pcache_state:=s; return ())
      }

      let { pcache_ops; c_pcache=_ } = 
        make_generic ~blk_ops ~blk_alloc ~with_pcache ~flush_tl 

    end
    in
    A.(Res2 { pcache_ops; pcache_ref=pcache_state; min_free_blk })




let _ :
('a blk_ops, unit -> (blk_id, lwt) m,
 ((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state, lwt)
 with_state,
 (blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state -> (unit, lwt) m)
make_args ->
((int, int, blk_id, (int, (int, int) kvop, 'b) Tjr_map.map, lwt) pcache_ops,
 (blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) pcache_state ref, int)
make_result
 = make
*)

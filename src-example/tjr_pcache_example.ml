(** Pcache example *)

open Make_

module Make_with_fixed_types = Make_with_fixed_types

module Blk_id = Blk_id_as_int
type blk_id = Blk_id.blk_id [@@deriving bin_io]

module type MC' = MC with type k = int and type v = int and type r = blk_id

module Int_int_marshalling_config : MC' = struct
  open Bin_prot.Std
  type k = int [@@deriving bin_io]
  type v = int [@@deriving bin_io]  
  type r = blk_id [@@deriving bin_io]
  type kvop = (k,v)Kvop.kvop [@@deriving bin_io]

  (** This is the max # of bytes required for k *)
  let k_size = 9
  let v_size = 9
  let r_size = 9  
end

let int_int_marshalling_config = (module Int_int_marshalling_config : MC')

let make_kvop_map_ops () = 
  let map_ops = Kv_op.default_kvop_map_ops () in
  let merge ~older ~newer = Tjr_map.map_merge ~map_ops ~old:older ~new_:newer in
  let Tjr_map.{ empty; find_opt; add; remove; _ } = map_ops in
  { empty; find_opt; insert=add; delete=remove; merge }

let _ = make_kvop_map_ops


type ('a,'b) make_t = {
  c_dmap: 'a;
  dmap_ops: 'b
}

(** Construct an int->int example *)
let make_generic ~blk_ops ~blk_alloc ~with_dmap ~write_to_disk = 
  let kvop_map_ops = make_kvop_map_ops () in
  let c_dmap = { 
    monad_ops=lwt_monad_ops;
    marshalling_config=int_int_marshalling_config;
    kvop_map_ops;
    blk_ops;
    blk_alloc;
    with_dmap;
    write_to_disk
  }
  in
  let dmap_ops = Make_.make c_dmap in
  {c_dmap; dmap_ops}

(* FIXME why is blk_ops generic here? *)
let _ :
blk_ops:'a blk_ops ->
blk_alloc:(unit -> (blk_id, lwt) m) ->
with_dmap:((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state, lwt)
          with_state ->
write_to_disk:((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state ->
               (unit, lwt) m) ->
((lwt monad_ops, (module MC'),
  (int, (int, int) kvop, (int, (int, int) kvop, 'b) Tjr_map.map)
  pcache_map_ops, 'a blk_ops, unit -> (blk_id, lwt) m,
  ((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state, lwt)
  with_state,
  (blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state ->
  (unit, lwt) m)
 c_dmap,
 (int, int, blk_id, (int, (int, int) kvop, 'b) Tjr_map.map, lwt) dmap_ops)
make_t
 = make_generic

;;

type ('a,'b,'c,'d) make_args = 
  | Make1: { blk_ops:'a; blk_alloc:'b; with_dmap:'c; write_to_disk:'d } -> ('a,'b,'c,'d) make_args
  (** generic version *)


  | Make2: { write_to_disk:'d } -> ('a,'x1,'x2,'d) make_args
  (** version with blk_ops std; dmap as a reference, and blk_alloc as a reference *)

[@@@ocaml.warning "-30"] (* duplicate record labes in mut rec type defn *)

type ('a,'b,'c) make_result = 
  | Res1: 'a aux1 -> ('a,'x1,'x2) make_result
  | Res2: ('a,'b,'c) aux2 -> ('a,'b,'c) make_result

and 'a aux1 = { dmap_ops:'a }

and ('a,'b,'c) aux2 = { dmap_ops: 'a; dmap_ref: 'b; min_free_blk:'c ref }

let dest_Res1 (Res1 x) = x[@@ocaml.warning "-8"]
let dest_Res2 (Res2 x) = x[@@ocaml.warning "-8"]


(** NOTE this takes Make1 to Res1, Make2 to Res2 etc *)     
let make = function
  | Make1 { blk_ops; blk_alloc; with_dmap; write_to_disk } -> (
    make_generic ~blk_ops ~blk_alloc ~with_dmap ~write_to_disk |> fun {dmap_ops; c_dmap=_ } ->
    Res1 { dmap_ops })
  | Make2 { write_to_disk } -> 
    let module A = struct
      open Tjr_monad.With_lwt

      let blk_ops = Common_blk_ops.string_blk_ops

      let min_free_blk = ref 1

      let blk_alloc () =     
        (* Printf.printf "blk_alloc: %d\n" !min_free_blk; *)
        let r = !min_free_blk  in
        incr min_free_blk;
        return (Blk_id.of_int r)

      let kvop_map_ops = Kv_op.default_kvop_map_ops ()

      let blk_sz = 4096

      let dmap_state = ref {
          root_ptr=Blk_id.of_int 0;
          past_map=kvop_map_ops.empty;
          current_ptr=Blk_id.of_int 0;
          current_map=kvop_map_ops.empty;
          buf=buf_ops.create blk_sz;
          buf_pos=0;
          next_ptr=None;
          block_list_length=1;
          dirty=true
        }

      let _ = check_state (!dmap_state)

      let with_dmap = {
        with_state=fun f -> 
          f ~state:!dmap_state 
            ~set_state:(fun s -> 
              check_state s;
              dmap_state:=s; return ())
      }

      let { dmap_ops; c_dmap=_ } = 
        make_generic ~blk_ops ~blk_alloc ~with_dmap ~write_to_disk 

    end
    in
    A.(Res2 { dmap_ops; dmap_ref=dmap_state; min_free_blk })




let _ :
('a blk_ops, unit -> (blk_id, lwt) m,
 ((blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state, lwt)
 with_state,
 (blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state -> (unit, lwt) m)
make_args ->
((int, int, blk_id, (int, (int, int) kvop, 'b) Tjr_map.map, lwt) dmap_ops,
 (blk_id, (int, (int, int) kvop, 'b) Tjr_map.map) dmap_state ref, int)
make_result
 = make

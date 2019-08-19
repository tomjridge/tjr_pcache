(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.Imperative

let usage = Printf.sprintf {|
Usage:

%s <filename> <count>

  - filename is the name of the store
  - count is the number of operations to insert
|} (Filename.basename Sys.executable_name)


(* allow float representation *)
let int_of_string s = 
  float_of_string_opt s |> function
  | None -> int_of_string s
  | Some f -> int_of_float f

let fn,count = 
  Array.to_list Sys.argv |> fun xs ->
  match xs with
  | [_; fn; count] -> fn,int_of_string count
  | _ -> (print_endline usage; Pervasives.exit (-1); "",-1)[@@ocaml.warning "-21"]


(* FIXME expose these in Imperative; rename to with_imperative *)
let ( >>= ) = imperative_monad_ops.bind
let return = imperative_monad_ops.return 


module Blk_id = Blk_id_as_int

module Pc = Tjr_pcache_example.Make_layers.With_imperative

let layers = Pc.make_int_int_layers ()

let blk_ops = layers.blk_ops

let min_free_blk = ref 1

let alloc () =     
  let r = Blk_id.of_int !min_free_blk in
  incr min_free_blk;
  return r

let blk_sz = 4096
    
module I = Tjr_pcache_example.Dmap_example.Initial_states

let with_ref ~ref f = 
  f ~state:!ref ~set_state:(fun s -> ref:=s;return ())

let _ = with_ref

let with_ref ref = {with_state=(fun f -> with_ref ~ref f)}

let pl = ref I.(
    let data = (create_buf blk_sz,0) in
    initial_pl_state ~data ~current:(Blk_id.of_int 0) ~next:None)

let with_pl = with_ref pl

(* FIXME buf size should match blk_sz *)
let pcl = ref I.(
    (* FIXME offset not int *)
    initial_pcl_state ~buf:(create_buf blk_sz) ~int:0)

let with_pcl = with_ref pcl

let dcl = ref I.(
    let r = Blk_id.of_int 0 in
    initial_dcl_state ~start_block:r ~current_block:r ~block_list_length:1
      ~past:[] ~current:[])

let with_dcl = with_ref dcl

let dmap = ref I.(
    initial_dmap_state ~dcl_state:!dcl)

let with_dmap = with_ref dmap

let fd = Tjr_file.fd_from_file ~fn ~create:true ~init:true 

let blk_dev_ops = layers.blk_dev_ops fd

let write_node = 
  let f = layers.write_node ~blk_dev_ops in
  fun pl_state -> f ~pl_state

let pcache_with = Tjr_pcache_example.Pcache_example_intf.{
    alloc;
    with_pl;
    with_pcl;
    with_dmap
  }

let dmap_ops = layers.dmap_ops ~pcache_with ~write_node

let _ = 
  measure_execution_time_and_print "run_pcache_example" @@ (fun () -> 
  0 |> iter_k (fun ~k i ->
      match i >= count with
      | true -> ()
      | false -> 
        of_m (dmap_ops.insert i (2*i));
        k (i+1)));
  Unix.close fd


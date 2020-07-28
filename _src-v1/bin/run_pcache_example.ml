(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.With_lwt
open Pcache_intf

let factory = Tjr_pcache_example.pcache_factory

module Blk_id = Blk_id_as_int

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
  | _ -> (print_endline usage; Stdlib.exit (-1); "",-1)[@@ocaml.warning "-21"]

let file_ops = lwt_file_ops

let pp_large_int i = 
  (i,"") |> iter_k (fun ~k (i,s) -> 
      match i = 0 with
      | true -> s
      | false -> 
        let r = i mod 1000 in
        let r = Printf.sprintf "%03d" r in
        let tl = if s="" then "" else "_"^s in
        k (i/1000,r^tl))


let main = 
  file_ops.open_ ~fn ~create:true ~init:true >>= fun fd ->
  let blk_dev_ops = blk_devs#with_ba_buf#from_fd fd |> fun o -> o#blk_dev_ops in
  
  let min_free_blk = ref (Blk_id.of_int 1) in

  let blk_alloc () =     
    (* Printf.printf "blk_alloc: %d\n" !min_free_blk; *)
    let r = !min_free_blk  in
    Blk_id.incr min_free_blk;
    return r
  in

  let fact = pcache_factory ~blk_alloc in

  let pcache = ref @@
    let r = (Blk_id.of_int 0) in
    fact#empty_pcache_state ~ptr:r
  in

  let with_pcache = with_ref pcache in

  let fact1 = fact#with_blk_dev_ops ~blk_dev_ops in

  let pcache_ops = fact1#make_pcache_ops#with_state with_pcache in

  let rec f n = 
    (* Printf.printf "n is %d\n%!" n; *)
    if n < count then pcache_ops.insert n (2*n) >>= fun () ->
      f (n+1)
    else return ()
  in
  let t0 = Tjr_profile.now() in
  f 0 >>= fun () ->
  pcache_ops.pcache_write () >>= fun () ->
  (* pcache_ops.read_pcache ~root:(Blk_id.of_int 0) ~read_blk_as_buf >>= fun xs -> *)
  (* Printf.printf "Number of blks: %d\n" (List.length xs); *)
  file_ops.close fd >>= fun () ->
  let t1 = Tjr_profile.now() in

  (* now attempt to read back *)
  let root_ptr,current_ptr = (!pcache).root_ptr, (!pcache).current_ptr in
  Printf.printf "pcache: reading back in, from %d to %d\n" 
    (Blk_id_as_int.to_int root_ptr) 
    (Blk_id_as_int.to_int current_ptr);
  file_ops.open_ ~fn ~create:false ~init:false >>= fun fd ->
  let blk_dev_ops = (blk_devs#with_ba_buf#from_fd fd)#blk_dev_ops  in
  let fact1 = fact#with_blk_dev_ops ~blk_dev_ops in
  fact1#read_initial_pcache_state root_ptr >>= fun s ->
  let t2 = Tjr_profile.now () in
  Printf.printf "pcache: read %d blocks\n" s.blk_len;
  Printf.printf "pcache: write_time/ns %s; read_time/ns %s\n" (pp_large_int (t1-t0)) (pp_large_int (t2-t1));
  return ()

let _ = Lwt_main.run (to_lwt main)


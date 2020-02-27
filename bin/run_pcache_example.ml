(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.With_lwt
open Pcache_intf.Pcache_state

module Ex = Tjr_pcache_example.Int_int_ex

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
  
  let min_free_blk = ref (Blk_id.of_int 1) in

  let blk_alloc () =     
    (* Printf.printf "blk_alloc: %d\n" !min_free_blk; *)
    let r = !min_free_blk  in
    Blk_id.incr min_free_blk;
    return r
  in

  let pcache = ref (
      let r = (Blk_id.of_int 0) in
      empty_pcache_state ~root_ptr:r ~current_ptr:r ~empty:Ex.kvop_map_ops.empty) in

  let with_pcache = with_ref pcache in

  (* FIXME why is this a separate parameter for pcache construction? *)
  let flush_tl s = 
    (* Printf.printf "Writing to disk with next pointer %d\n" (dest_Some s.next_ptr |> Blk_id.to_int);  *)
    file_ops.write_blk fd (Blk_id.to_int s.current_ptr) s.buf
  in

  let pcache_ops = Ex.make ~blk_alloc ~with_pcache ~flush_tl in

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
  let blk_dev_ops = Blk_dev_factory.make_5 fd in
  Ex.read_initial_pcache_state 
    ~read_blk_as_buf:(fun blk_id -> blk_dev_ops.read ~blk_id)
    ~root_ptr
    ~current_ptr >>= fun s ->
  let t2 = Tjr_profile.now () in
  Printf.printf "pcache: read %d blocks\n" s.blk_len;
  Printf.printf "pcache: write_time/ns %s; read_time/ns %s\n" (pp_large_int (t1-t0)) (pp_large_int (t2-t1));
  return ()

let _ = Lwt_main.run (to_lwt main)


(*
  measure_execution_time_and_print "run_pcache_example" @@ (fun () -> 
  0 |> iter_k (fun ~k i ->
      match i >= count with
      | true -> ()
      | false -> 
        of_m (pcache_ops.insert i (2*i));
        k (i+1)));
  Unix.close fd

  let _read_blk_as_buf r = 
    file_ops.read_blk fd (Blk_id.to_int r) >>= fun blk ->
    return (Bigstring.of_bytes blk)
  in

*)

(* FIXME implement async writes with pcache, or use vector writes
  let _async_flush_tl s = 
    Lwt.async (fun () -> to_lwt(file_ops.write_blk fd s.current_ptr (Bigstring.to_bytes s.buf)));
    return ()
  in
*)

(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.With_lwt
open Pcache_intf.Pvt

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

let main = 
  file_ops.open_ ~fn ~create:true ~init:true >>= fun fd ->
  
  let min_free_blk = ref (Blk_id.of_int 1) in

  let blk_alloc () =     
    (* Printf.printf "blk_alloc: %d\n" !min_free_blk; *)
    let r = !min_free_blk  in
    Blk_id.incr min_free_blk;
    return r
  in

  let dmap = ref (Ex.dmap0 ~r:(Blk_id.of_int 0)) in

  let with_dmap = with_ref dmap in

  (* FIXME why is this a separate parameter for pcache construction? *)
  let write_to_disk s = 
    (* Printf.printf "Writing to disk with next pointer %d\n" (dest_Some s.next_ptr |> Blk_id.to_int);  *)
    file_ops.write_blk fd (Blk_id.to_int s.current_ptr) s.buf
  in

  let dmap_ops = Ex.make ~blk_alloc ~with_dmap ~write_to_disk in

  let rec f n = 
    (* Printf.printf "n is %d\n%!" n; *)
    if n < count then dmap_ops.insert n (2*n) >>= fun () ->
      f (n+1)
    else return ()
  in
  f 0 >>= fun () ->
  dmap_ops.dmap_write () >>= fun () ->
  (* dmap_ops.read_pcache ~root:(Blk_id.of_int 0) ~read_blk_as_buf >>= fun xs -> *)
  (* Printf.printf "Number of blks: %d\n" (List.length xs); *)
  file_ops.close fd

let _ = Lwt_main.run (to_lwt main)


(*
  measure_execution_time_and_print "run_pcache_example" @@ (fun () -> 
  0 |> iter_k (fun ~k i ->
      match i >= count with
      | true -> ()
      | false -> 
        of_m (dmap_ops.insert i (2*i));
        k (i+1)));
  Unix.close fd

  let _read_blk_as_buf r = 
    file_ops.read_blk fd (Blk_id.to_int r) >>= fun blk ->
    return (Bigstring.of_bytes blk)
  in

*)

(* FIXME implement async writes with pcache, or use vector writes
  let _async_write_to_disk s = 
    Lwt.async (fun () -> to_lwt(file_ops.write_blk fd s.current_ptr (Bigstring.to_bytes s.buf)));
    return ()
  in
*)

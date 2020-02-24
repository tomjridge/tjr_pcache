(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.With_lwt
open Pvt_dmap_state

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

open Tjr_pcache_example

let main = 
  file_ops.fd_from_file ~fn ~create:true ~init:true >>= fun fd ->
(*
  let _async_write_to_disk s = 
    Lwt.async (fun () -> to_lwt(file_ops.write_blk fd s.current_ptr (Bigstring.to_bytes s.buf)));
    return ()
  in
*)
  let write_to_disk s = 
    (* Printf.printf "Writing to disk with next pointer %d\n" (dest_Some s.next_ptr |> Blk_id.to_int);  *)
    file_ops.write_blk fd (Blk_id.to_int s.current_ptr) (Bigstring.to_bytes s.buf) 
  in
  let _read_blk_as_buf r = 
    file_ops.read_blk fd (Blk_id.to_int r) >>= fun blk ->
    return (Bigstring.of_bytes blk)
  in
  let { dmap_ops; _ } = make (Make2 { write_to_disk } ) |> dest_Res2 in
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
*)

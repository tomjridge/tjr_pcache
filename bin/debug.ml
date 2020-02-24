(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.With_lwt
open Pvt_dmap_state

let usage = Printf.sprintf {|
Usage:

%s <filename> 

  - prints out the contents of an int*int pcache
|} (Filename.basename Sys.executable_name)

let fn = 
  Array.to_list Sys.argv |> fun xs ->
  match xs with
  | [_; fn] -> fn
  | _ -> (print_endline usage; Stdlib.exit (-1); "")[@@ocaml.warning "-21"]


let file_ops = lwt_file_ops

open Tjr_pcache_example


type ii_on_disk_node = ((int, int) kvop list * blk_id option) list [@@deriving yojson]

let main = 
  file_ops.fd_from_file ~fn ~create:false ~init:false >>= fun fd ->
(*
  let _async_write_to_disk s = 
    Lwt.async (fun () -> to_lwt(file_ops.write_blk fd s.current_ptr (Bigstring.to_bytes s.buf)));
    return ()
  in
*)
  let write_to_disk s = 
    Printf.printf "Writing to disk with next pointer %d\n" (dest_Some s.next_ptr |> Blk_id.to_int); 
    file_ops.write_blk fd (Blk_id.to_int s.current_ptr) (Bigstring.to_bytes s.buf) 
  in
  let read_blk_as_buf r = 
    file_ops.read_blk fd (Blk_id.to_int r) >>= fun blk ->
    return (Bigstring.of_bytes blk)
  in
  let { dmap_ops; _ } = make (Make2 { write_to_disk } ) |> dest_Res2 in
  dmap_ops.read_pcache ~root:(Blk_id.of_int 0) ~read_blk_as_buf >>= fun xs ->
  Printf.printf "Number of blks: %d\n" (List.length xs);
  (xs |> List.map (fun (ops,_) -> ops) |> List.concat |> List.length |> fun n -> 
     Printf.printf "Number of ops: %d\n" n);
  (* ii_on_disk_node_to_yojson xs |> Yojson.Safe.pretty_to_string |> print_endline; *)
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

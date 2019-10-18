(** Test the pcache backed by a file. Command line arg is number of
   insert operations to try. *)

open Tjr_monad.With_lwt

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


let file_ops = lwt_file_ops

let blk_ops = Common_blk_ops.string_blk_ops

let min_free_blk = ref 1

let blk_alloc () =     
  (* Printf.printf "blk_alloc: %d\n" !min_free_blk; *)
  let r = !min_free_blk in
  incr min_free_blk;
  return r

let map_ops = make_map_ops Pervasives.compare

let blk_sz = 4096

let dmap_state = ref {
    root_ptr=0;
    past_map=map_ops.empty;
    current_ptr=0;
    current_map=map_ops.empty;
    buf=buf_ops.create blk_sz;
    buf_pos=0;
    next_ptr=None;
    block_list_length=1;
    dirty=true
  }

let with_dmap = {
  with_state=fun f -> 
  f ~state:!dmap_state ~set_state:(fun s -> dmap_state:=s; return ())
}

let main = 
  file_ops.fd_from_file ~fn ~create:true ~init:true >>= fun fd ->
  let _async_write_to_disk s = 
    Lwt.async (fun () -> to_lwt(file_ops.write_blk fd s.current_ptr (Bigstring.to_bytes s.buf)));
    return ()
  in
  let write_to_disk s = file_ops.write_blk fd s.current_ptr (Bigstring.to_bytes s.buf) in
  let _,dmap_ops = 
    Tjr_pcache_example.make ~blk_ops ~blk_alloc ~with_dmap ~write_to_disk in
  let rec f n = 
    if n < count then dmap_ops.insert n (2*n) >>= fun () ->
      f (n+1)
    else return ()
  in
  f 0 >>= fun () ->
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

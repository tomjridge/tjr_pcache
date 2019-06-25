(** Test the dmap backed by a file *)
open Tjr_pcache

let fn = "dmap_example.store"
let count = int_of_string Sys.argv.(1)

let now =
  Core.(fun () ->
      Time_stamp_counter.(now () |> to_time_ns)
      |>Time_ns.to_int63_ns_since_epoch 
      |>Int63.to_int_exn)

let _ = 
  Printf.printf "%s: starting write... %!" __MODULE__;
  let now1 = now() in
  Dmap_example.test_dmap_ops_on_file ~fn ~count;
  let time = now() - now1 in
  Printf.printf "finished in %d\n%!" time

let pow =
  let rec pow' a x n =
    if n = 0 then a else pow' (a * (if n mod 2 = 0 then 1 else x)) (x * x) (n / 2) in
  pow' 1

let _ =
  Printf.printf "%s: starting read... %!" __MODULE__;
  let now1 = now() in
  Dmap_example.read_back ~fn |> fun ess ->
  let time = now() - now1 in
  Printf.printf 
    " read back %d ops in %d\n%!" 
    (List.length (List.concat ess)) 
    time;
  
(*
let open open Ins_del_op_type in
  let ess = List.concat ess in
  let _ess' = 
    ess
    |> List.rev 
    |> Tjr_list.take 100
    |> List.rev
  in
  let ess'' = 
    ess
    |> Tjr_list.take 100
  in
  List.iter (fun op ->
      match op with
      | Insert(k,v) -> Printf.printf "Insert(%d,%d)\n%!" k v
      | _ -> ()
    )
    ess''
*)

(*

100k in 2.2s

1M:
Run_dmap_example: starting write... finished in 27924296012  
ie 27 s to write

Run_dmap_example: starting read...  read back 1000000 ops in 189117339
.18s to read

*)

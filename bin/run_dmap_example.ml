(** Test the dmap backed by a file *)
open Tjr_pcache

let fn = "dmap_example.store"
let count = int_of_string Sys.argv.(1)

let _ = 
  Dmap_example.test_dmap_ops_on_file ~fn ~count

let pow =
  let rec pow' a x n =
    if n = 0 then a else pow' (a * (if n mod 2 = 0 then 1 else x)) (x * x) (n / 2) in
  pow' 1

let _ =
  Dmap_example.read_back ~fn |> fun ess ->
  Printf.printf "%s: read back %d ops\n%!" __MODULE__ (List.length (List.concat ess));
  
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
1M fails with stackoverflow - presumably a list op somewhere is not tail rec? culprit seems to be Tjr_list.from_to

*)

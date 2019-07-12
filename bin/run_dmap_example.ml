(** Test the dmap backed by a file *)

let profiler = Tjr_profile.make_string_profiler ()  


(* allow float representation *)
let int_of_string s = 
  float_of_string_opt s |> function
  | None -> int_of_string s
  | Some f -> int_of_float f

(* FIXME use config *)
let fn = "dmap_example.store"
let count = int_of_string Sys.argv.(1)


let _ = 
  measure_execution_time_and_print "run_dmap_example" @@ fun () -> 
  Dmap_example.Test.test_dmap_ops_on_file ~fn ~count

module Internal = struct
  type t = (int,int)Pcache_intf.op list list[@@deriving yojson]
end

let _ =
  measure_execution_time_and_print "read_back" @@ fun () -> 
  Dmap_example.Internal_read_node.read_back ~fn |> fun ess ->
  Printf.printf "read back %d ops\n%!" (List.length (List.concat ess))
(*
  Printf.sprintf "Read back: %s\n" (ess |> Internal.to_yojson |> Yojson.Safe.pretty_to_string) |> fun s ->
  Tjr_file.write_string_to_file ~fn:"tmp.txt" s
*)

  
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

open Tjr_pcache

let _ =
  Pervasives.at_exit @@ fun () -> 
  print_endline (__LOC__ ^ ": running exit hooks");
  Tjr_fs_shared.Fs_log.log_ops.Tjr_log.print_last_n ()


(* default is to run all tests; turn tests off individually by giving
   their names on the cl *)
let args = Array.to_list Sys.argv

let _ = 
  if List.mem "pl" args then () else ignore(Pl_test.main());
  if List.mem "pcl" args then () else ignore(Pcl_test.main());
  if List.mem "dcl" args then () else ignore(Dcl_test.test ~depth:6)



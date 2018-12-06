(* open Tjr_pcache *)
open Tjr_pcache_test
module Logger = Tjr_fs_shared.Logger  
  
(* default is to run all tests; turn tests off individually by giving
   their names on the cl *)
let args = Array.to_list Sys.argv

let _ = 
  Logger.at_exit ~print:true;
  if List.mem "pl" args then () else ignore(Pl_test.main());
  if List.mem "pcl" args then () else ignore(Pcl_test.main());
  if List.mem "dcl" args then () else ignore(Dcl_test.test ~depth:6);
  Logger.at_exit ~print:false



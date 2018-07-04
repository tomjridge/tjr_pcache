open Tjr_pcache

(* default is to run all tests; turn tests off individually by giving their names on the cl *)
let args = Array.to_list Sys.argv

let _ = 
  if List.mem "pl" args then () else
  ignore(Persistent_list.Test.main());
  ignore(Persistent_chunked_list.Test.main());
  Persistent_log.Test.test ~depth:6


module Gom_requires = struct
  module Bt_blk_id = Tjr_int.Make_type_isomorphic_to_int()
  module Pc_blk_id = Tjr_int.Make_type_isomorphic_to_int()
end


module Gom = Gom.Make_gom(Gom_requires)

module Test = Gom.Test()


let _ = 
  if List.mem "gom" args then () else
  Test.run_tests ~depth:2

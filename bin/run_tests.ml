open Tjr_pcache

let _ = 
  ignore(Persistent_list.Test.main());
  ignore(Persistent_chunked_list.Test.main());
  Persistent_log.Test.test ~depth:6

open Tjr_pcache_test
    
let _ = 
  Alcotest.run "Pcache tests" [ 
    "pl", Pl_test.test_set;
    (* FIXME "pcl",Pcl_test.test_set *)
  ]



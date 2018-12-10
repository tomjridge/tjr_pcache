(** Test the dcl backed by a file *)
open Tjr_pcache

let _ = 
  Dcl_example.test_dcl_ops_on_file ~fn:"test_dcl_ops.store" ~count:100000

(*

10k in .48s
100k in 4s

*)

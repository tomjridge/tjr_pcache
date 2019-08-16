(* FIXME FIXME 


(** Test the dmap backed by a file. Command line arg is number of
   insert operations to try. *)

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
  Dmap_example.With_fstore.Test.test_dmap_ops_on_file ~fn ~count
*)

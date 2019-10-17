(** A monad which passes a functional store. FIXME move elsewhere?
   NOTE used for testing dmap etc, but common in other repos *)

let monad_ops = Fstore_passing.monad_ops

let ( >>= ) = monad_ops.bind 
let return = monad_ops.return


(* FIXME? used to have an f argument *)
let with_ref r = Fstore_passing.fstore_ref_to_with_state r

let run ~init_state m = 
  (sp_to_fun m) init_state


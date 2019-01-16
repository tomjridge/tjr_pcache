(** A monad which passes a functional store. FIXME move elsewhere? *)

open Tjr_monad
open Monad_ops
open State_passing
open Tjr_store

let monad_ops : Tjr_store.t state_passing monad_ops = 
  Tjr_monad.State_passing.monad_ops'

let ( >>= ) = monad_ops.bind 
let return = monad_ops.return


let with_ref r f = Tjr_monad.State_passing.with_state
    ~get:(fun x -> get r x) 
    ~set:(fun s t -> set r s t)
    ~f

(*
let mk_ref' v = 
  !test_store |> fun t ->
  Tjr_store.mk_ref v t |> fun (t,r) ->
  test_store:=t;
  r
*)

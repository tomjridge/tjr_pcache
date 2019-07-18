(** A basic implementation of a block device; runtime checks for correct block size *)

(* open Store_passing *)
open Tjr_pcache.Pcache_intf

module Profiler = Tjr_profile.With_array.Make_profiler(struct let cap = int_of_float 1e7 end)
open Profiler

module Internal2 = struct
  let [write;write'] = 
    List.map Profiler.allocate_int 
      ["write";"write'"]
[@@warning "-8"]
end
open Internal2

(* FIXME there is a version of this in fs_shared *)
module Internal = struct

  type fd = Unix.file_descr

  (* open Util  *)


  (* raw operations --------------------------------------------------- *)

  let read ~fd ~blk_sz ~blk_id = 
    ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
    let buf = Bytes.make blk_sz (Char.chr 0) in 
    let n = Unix.read fd buf 0 blk_sz in
    (* assert (n=blk_sz); we allow the file to expand automatically, so
       no reason to read any bytes since file could be empty *)
    (* test(fun _ -> assert(n=0 || n=blk_sz)); *)
    assert(n=0 || n=blk_sz);
    Bytes.to_string buf

  let write ~fd ~blk_sz ~blk_id ~blk = 
    mark write;
    assert(String.length blk > 0);
    assert(blk_sz > 0);
    ignore (Unix.lseek fd (blk_id * blk_sz) SEEK_SET);
    let buf = Bytes.of_string blk in
    (* Printf.printf "%d %d\n%!" (String.length blk) blk_sz; *)
    let n = Unix.single_write fd buf 0 blk_sz in
    (* test(fun _ -> assert (n=blk_sz)); *)
    assert (n=blk_sz);
    mark write';
    ()

  (** Construct [blk_dev_ops] *)
  let make_blk_dev_on_file ~monad_ops ~blk_sz = 
    (* let ( >>= ) = monad_ops.bind in *)
    let return = monad_ops.return in
    let read ~dev:fd ~blk_id = 
      return (read ~fd ~blk_sz ~blk_id)
    in
    let write ~dev:fd ~blk_id ~blk = 
      return (write ~fd ~blk_sz ~blk_id ~blk)
    in
    { write;read }

  let _ = make_blk_dev_on_file


end


let make_blk_dev_on_file = Internal.make_blk_dev_on_file
